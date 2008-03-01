;;; bit-io.scm  --  Jens Axel Søgaard -- april 2006

; This file started as a PLT port of Oleg's bit-reader
;    <http://okmij.org/ftp/Scheme/binary-io.html#bit-reader>,
; then a bit-writer was added, and finally bit-ports were
; added.

(module bit-io mzscheme
  (provide  with-input-from-bit-file
            with-output-to-bit-file
            open-input-bit-file
            open-output-bit-file
            close-input-bit-port
            close-output-bit-port
            read-bits
            write-bits
            current-output-bit-port
            current-input-bit-port
            flush-bits
            bit-file-position
            call-with-input-bit-file
            call-with-output-bit-file
            ; LOW LEVEL
            ;make-bit-reader
            ;make-bit-writer
            ;current-bit-reader
            ;current-bit-writer
            ;current-bit-flusher
            )
  
  ; A BIT-PORT consists of an underlying (byte) port and
  ; the current (bit)position.
  (define-struct bit-port (port pos))
  
  ; An INPUT-BIT-PORT is a bit-port with a concrete byte-reader
  ; which from which the corresponding bit-reader read is constructed.
  (define-struct (input-bit-port bit-port)  (byte-reader read))
  
  ; An OUTPUT-BIT-PORT is a bit-port with a concrete byte-writer
  ; from which the corresponding bit-writer write is constructed.
  ; Bits aren't written to the underlying byte port until a whole
  ; byte is received - a flush operations is thus sometimes needed
  ; at the end of a file.
  (define-struct (output-bit-port bit-port) (byte-writer write flush))
  
  ; open-input-bit-file : path [symbol ... ] -> input-bit-port
  ;   analog to open-input-file
  (define (open-input-bit-file file . options)
    (let ([byte-port (apply open-input-file file options)])
      (make-input-bit-port byte-port 
                           0
                           (λ () (read-byte byte-port))
                           (make-bit-reader (λ () (read-byte byte-port))))))
  
  ; open-output-bit-file : path [symbol ...] -> output-bit-port
  ;   analog to open-output-file
  (define (open-output-bit-file file . options)
    (let ([byte-port (apply open-output-file file options)])
      (let-values ([(bit-writer bit-flusher) 
                    (make-bit-writer (λ (b) (write-byte b byte-port)))])
        (make-output-bit-port byte-port 
                              0
                              (λ (b) (write-byte b byte-port))
                              bit-writer
                              bit-flusher))))
  
  ; current bit-ports
  (define current-input-bit-port  (make-parameter 'none-yet))
  (define current-output-bit-port (make-parameter 'none-yet))
  
  ; close-input-bit-port : input-bit-port -> 
  (define (close-input-bit-port bit-port)
    (close-input-port (bit-port-port bit-port)))
  
  ; close-output-bit-port : output-bit-port ->
  (define (close-output-bit-port bit-port)
    ((output-bit-port-flush bit-port))
    (close-output-port (bit-port-port bit-port)))
  
  ; with-input-from-bit-file : path (-> alpha) [symbol ...] -> alpha
  ;   analog to with-input-from-file
  (define (with-input-from-bit-file file thunk . options)
    (let ([in (apply open-input-bit-file file options)])
      (begin0
        (parameterize ([current-input-bit-port in])
          (thunk))
        (close-input-bit-port in))))
  
  ; with-output-to-bit-file : path (-> alpha) [symbol ...] -> alpha
  ;   analog to with-to-file
  (define (with-output-to-bit-file file thunk . options)
    (let ([out (apply open-output-bit-file file options)])
      (begin0
        (parameterize ([current-output-bit-port out])
          (thunk))
        (close-output-bit-port out))))
  
  (define (call-with-input-bit-file file proc . options)
    (let ([in (apply open-input-bit-file file options)])
      (begin0
        (proc in))
      (close-input-bit-port in)))
  
  (define (call-with-output-bit-file file proc . options)
    (let ([out (apply open-output-bit-file file options)])
      (begin0
        (proc out)
        (close-output-bit-port out))))
  
  ; write-bits : natural natural [output-bit-port] -> 
  ;   write n lower bits from bs to the output-bit-port,
  ;   if no output-bit-port is given use current-output-bit-port
  (define write-bits
    (case-lambda 
      [(n bs)          (write-bits n bs (current-output-bit-port))]
      [(n bs bit-port) (begin
                         (set-bit-port-pos! bit-port (+ (bit-port-pos bit-port) n))
                         ((output-bit-port-write bit-port) n bs))]))
  
  ; read-bits : natural [input-bit-port] -> natural
  ;   read n bits from the input-bit-port,
  ;   if no input-bit-port is given, use current-input-bit-port
  (define read-bits
    (case-lambda 
      [(n)          (read-bits n (current-input-bit-port))]
      [(n bit-port) (begin
                      (set-bit-port-pos! bit-port (+ (bit-port-pos bit-port) n))
                      ((input-bit-port-read bit-port) n))]))
  
  ; flush-bits : [output-bit-port] ->
  ;    flush remaining bits in the cache by append zeros until a
  ;    whole byte can be written
  (define flush-bits
    (case-lambda
      [()             (flush-bits (current-output-bit-port))]
      [(out-bit-port) ((output-bit-port-flush out-bit-port))]))

  ; bit-file-position : bit-port -> natural
  ;   return the bit-position of the bit-port,
  ;   the bit-position of an input-bit-port is the number of bits read so far,
  ;   for an output-bit-port it is the number of bits written so far
  ; bit-file-position : bit-port natural -> natural
  ;   set the bit-position of the bit-port
  (define bit-file-position
    ; todo: this only sets the position on an input port
    (case-lambda 
      [(bit-port)   (bit-port-pos bit-port)]
      [(bit-port n) (begin
                      (unless (input-bit-port? bit-port)
                        (error 
                         (string-append 
                          "(bit-file-position bit-port n) is not implemented "
                          "for output bit ports.")))
                      (file-position (bit-port-port bit-port) (quotient n 8))
                      (read-bits (remainder n 8) bit-port))]))
  
  ;;;
  ;;; LOW LEVEL INTERFACE
  ;;;
  
  ; The following bit reader comes from 
  ;     <http://okmij.org/ftp/Scheme/binary-io.html#bit-reader>
  
  ; Binary parsing
  
  ;----------------------------------------
  ; Apologia
  ;
  ; Binary parsing and unparsing are transformations between primitive or
  ; composite Scheme values and their external binary representations.
  ;
  ; Examples include reading and writing JPEG, TIFF, MP3, ELF file
  ; formats, communicating with DNS, Kerberos, LDAP, SLP internet
  ; services, participating in Sun RPC and CORBA/IIOP distributed systems,
  ; storing and retrieving (arrays of) floating-point numbers in a
  ; portable and efficient way. This project will propose a set of low- and
  ; intermediate- level procedures that make binary parsing possible.
  
  ; Scheme is a good language to do research in text compression. Text
  ; compression involves a great deal of building and traversing
  ; dictionaries, trees and similar data structures, where Scheme
  ; excels. Performance doesn't matter in research, but the size of
  ; compressed files does (to figure out the bpc for the common
  ; benchmarks). Variable-bit i/o is a necessity. It is implemented
  ; in the present file.
  
  ; ASN.1 corresponds to a higher-level parsing (LR parser
  ; vs. lexer). Information in LDAP responses and X.509 certificates is
  ; structural and recursive, and so lends itself to be processed in
  ; Scheme. Variable bit i/o is necessary, and so is a binary lexer for
  ; a LR parser. Parsing of ASN.1 is a highly profitable enterprise
  
  ;----------------------------------------
  ; The outline of the project
  ;
  ; Primitives and streams
  ;
  ; - read-byte 
  ; - read-u8vector (cf. read-string)
  ; - with-input-from-u8vector, with-input-from-encoded-u8vector 'base64,... 
  ; building binary i/o streams from a sequence of bytes. Streams over
  ; u8vector, u16vector, etc. provide a serial access to memory. See SRFI-4
  ;
  ; - read-bit, read-bits via overlayed streams given read-byte
  ; implemented in the present file.
  ;
  ; -  mmap-u8vector, munmap-u8vector
  ;
  ; Conversions
  ;  - u8vector->integer u8vector endianness,
  ;    u8vector->sinteger u8vector endianness
  ;  These conversion procedures turn a sequence of bytes to an unsigned or
  ;  signed integer, minding the byte order. The u8vector in question can
  ;  have size 1,2,4,8, 3 etc. bytes. These two functions therefore can be
  ;  used to read shorts, longs, extra longs, etc. numbers.
  ;  - u8vector-reverse and other useful u8vector operations
  ; 
  ;  - modf, frexp, ldexp
  ;  The above primitives can be emulated in R5RS, yet they are quite handy
  ;  (for portable FP manipulation) and can be executed very efficiently by
  ;  an FPU.
  ;
  ; Higher-level parsing and combinators
  ; These are combinators that can compose primitives above for more
  ; complex (possibly iterative) actions. 
  ;
  ; - skip-bits, next-u8token,...
  ; - IIOP, RPC/XDR, RMI
  ; - binary lexer for existing LR/LL-parsers
  ; 
  ; The composition of primitives and combinators will represent binary
  ; parsing language in a _full_ notation. This is similar to XPath
  ; expressions in full notation. Later we need to find out the
  ; most-frequently used patterns of the binary parsing language and
  ; design an abbreviated notation. The latter will need a special
  ; "interpreter". The abbreviated notation may turn out to look like
  ; Olin's regular expressions.
  
  
  ;;========================================================================
  ;;			Configuration section
  ;;
  ; Performance is very important for binary parsing. We have to get all
  ; help from a particular Scheme system we can get. If a Scheme function
  ; can support the following primitives faster, we should take
  ; advantage of that fact.
  
  ;; Configuration for PLT
  
  (define-syntax <<  (syntax-rules () [(_ x n) (arithmetic-shift x n)]))
  (define-syntax >>  (syntax-rules () [(_ x n) (arithmetic-shift x (- n))]))
  (define-syntax <<1 (syntax-rules () [(_ x)   (arithmetic-shift x 1)]))
  (define-syntax >>1 (syntax-rules () [(_ x)   (arithmetic-shift x -1)]))
  
  (define-syntax bit-set? (syntax-rules () [(_ x mask) (not (zero? (bitwise-and x mask)))]))
  
  ;; End configuration for PLT
  
  ; combine bytes in the MSB order. A byte may be #f
  (define (combine-two b1 b2)		; The result is for sure a fixnum
    (and b1 b2 (bitwise-ior (<< b1 8) b2)))
  
  (define (combine-three b1 b2 b3)	; The result is for sure a fixnum
    (and b1 b2 b3 (bitwise-ior (<< (bitwise-ior (<< b1 8) b2) 8) b3)))
  
  ; Here the result may be a BIGNUM
  (define (combine-bytes . bytes)
    (cond 
      ((null? bytes) 0)
      ((not (car bytes)) #f)
      (else 
       (let loop ((bytes (cdr bytes)) (result (car bytes)))
         (cond
           ((null? bytes) result)
           ((not (car bytes)) #f)
           (else (loop (cdr bytes) (+ (car bytes) (* 256 result)))))))))
  
  ;========================================================================
  ;			   Reading a byte 
  
  ; Read-byte is a fundamental primitive; it needs to be
  ; added to the standard. Most of the other functions are library
  ; procedures. The following is an approximation, which clearly doesn't
  ; hold if the port is a Unicode (especially UTF-8) character stream.
  
  ; The mzscheme read-byte is used.
  
  ; Return a byte as an exact integer [0,255], or the EOF object
  #;(define (read-byte port)
      (let ((c (read-char port)))
        (if (eof-object? c) c (char->integer c))))
  
  ; The same as above, but returns #f on EOF.
  (define (read-byte-f port)
    (let ([b (read-byte port)])
      (and (not (eof-object? b)) b)))
  
  
  ;========================================================================
  ;			Bit stream
  
  ; -- Function: make-bit-reader BYTE-READER
  
  ; Given a BYTE-READER (a thunk), construct and return a function
  ;	bit-reader N
  ;
  ; that reads N bits from a byte-stream represented by the BYTE-READER.
  ; The BYTE-READER is a function that takes no arguments and returns
  ; the current byte as an exact integer [0-255]. The byte reader
  ; should return #f on EOF.
  ; The bit reader returns N bits as an exact unsigned integer, 
  ; 0 -... (no limit). N must be a positive integer, otherwise the bit reader
  ; returns #f. There is no upper limit on N -- other than the size of the
  ; input stream itself and the amount of (virtual) memory an OS is willing
  ; to give to your process. If you want to read 1M of _bits_, go ahead.
  ;
  ; It is assumed that the bit order is the most-significant bit first.
  ;
  ; Note the bit reader keeps the following condition true at all times:
  ;	(= current-inport-pos (ceiling (/ no-bits-read 8)))
  ; That is, no byte is read until the very moment we really need (some of)
  ; its bits. The bit reader does _not_ "byte read ahead".
  ; Therefore, it can be used to handle a concatenation of different
  ; bit/byte streams *STRICTLY* sequentially, _without_ 'backing up a char',
  ; 'unreading-char' etc. tricks.
  ; For example, make-bit-reader has been used to read GRIB files of
  ; meteorological data, which made of several bitstreams with headers and
  ; tags.
  ; Thus careful attention to byte-buffering and optimization are the
  ; features of this bit reader.
  ;
  ; Usage example:
  ;	(define bit-reader (make-bit-reader (lambda () #b11000101)))
  ;	(bit-reader 3) ==> 6
  ;	(bit-reader 4) ==> 2
  ; The test driver below is another example.
  ;
  ; Notes on the algorithm.
  ; The function recognizes and handles the following special cases:
  ;  - the buffer is empty and 8, 16, 24 bits are to be read
  ;  - reading all bits which are currently in the byte-buffer
  ;    (and then maybe more)
  ;  - reading only one bit
  
  ; Since the bit reader is going to be called many times, optimization is
  ; critical. We need all the help from the compiler/interpreter
  ; we can get.
  
  (define (make-bit-reader byte-reader)
    (let ((buffer 0) (mask 0)     ; mask = 128 means that the buffer is full and
                     ; the msb bit is the current (yet unread) bit
                     (bits-in-buffer 0))
      
      ; read the byte into the buffer and set up the counters.
      ; return #f on eof
      (define (set-buffer)
        (set! buffer (byte-reader))
        (and buffer
             (begin
               (set! bits-in-buffer 8)
               (set! mask 128)
               #t)))
      
      ; Read fewer bits than there are in the buffer
      (define (read-few-bits n)
        (let ((value (bitwise-and buffer 	; all bits in buffer
                                  (sub1 (<<1 mask)))))
          (set! bits-in-buffer (- bits-in-buffer n))
          (set! mask (>> mask n))
          (>> value bits-in-buffer))) ; remove extra bits
      
      ; read n bits given an empty buffer, and append them to value, n>=8
      (define (add-more-bits value n)
        (let loop ((value value) (n n))
          (cond
	    ((zero? n) value)
	    ((< n 8)
	     (let ((rest (read-n-bits n)))
	       (and rest (+ (* value (<< 1 n)) rest))))
	    (else
	     (let ((b (byte-reader)))
	       (and b (loop (+ (* value 256) b) (- n 8))))))))
      
      ; The main module
      (define (read-n-bits n)
        ; Check the most common cases first
        (cond
	  ((not (positive? n)) #f)
	  ((zero? bits-in-buffer)	; the bit-buffer is empty
	   (case n
	     ((8) (byte-reader))
	     ((16) 
	      (let ((b (byte-reader)))
		(combine-two b (byte-reader))))
	     ((24)
	      (let* ((b1 (byte-reader)) (b2 (byte-reader)))
		(combine-three b1 b2 (byte-reader))))
	     (else
	      (cond
                ((< n 8)
                 (and (set-buffer) (read-few-bits n)))
                ((< n 16)
                 (let ((b (byte-reader)))
                   (and (set-buffer)
                        (bitwise-ior (<< b (- n 8))
                                     (read-few-bits (- n 8))))))
                (else
                 (let ((b (byte-reader)))
                   (and b (add-more-bits b (- n 8)))))))))
          
	  ((= n 1)			; read one bit
	   (let ((value (if (bit-set? buffer mask) 1 0)))
	     (set! mask (>>1 mask))
	     (set! bits-in-buffer (sub1 bits-in-buffer))
	     value))
          
	  ((>= n bits-in-buffer)	; will empty the buffer
	   (let ((n-rem (- n bits-in-buffer))
		 (value (bitwise-and buffer 	; for mask=64, it'll be &63
                                     (sub1 (<<1 mask)))))
	     (set! bits-in-buffer 0)
	     (cond
               ((zero? n-rem) value)
               ((<= n-rem 16)
                (let ((rest (read-n-bits n-rem)))
                  (and rest (bitwise-ior (<< value n-rem) rest))))
               (else (add-more-bits value n-rem)))))
	  (else (read-few-bits n))))
      read-n-bits)
    )
  
  ;;;
  ;;; BIT WRITER
  ;;;
  
  ; -- Function: make-bit-writer BYTE-WRITER
  
  ; Given a BYTE-WRITER (function of one argument), construct and return a function
  ;	bit-writer N B
  ;
  ; that writes N bits represented by the integer B to a byte-stream represented 
  ; by the BYTE-WRITER.
  ; The BYTE-WRITER is a function that takes one argument and writes
  ; the given byte as an exact integer [0-255]. 
  
  ; It is assumed that the bit order is the most-significant bit first.
  ;
  ; Note the bit writer will output bytes as soon as possible. That is
  ; the maximum number of waiting bits are 7. Call bit-writer with a
  ; non-number as argument to flush the remainin bits.
  
  (define (make-bit-writer byte-writer)
    (let ((buffer 0) 
          (bits-in-buffer 0))
      
      (define (empty-buffer!)
        (set! buffer 0)
        (set! bits-in-buffer 0))
      
      (define (low-bits n b)
        (bitwise-and b (vector-ref #(0 1 3 7 15 31 63 127 255) n)))
      
      (define (extend-buffer! n b)
        (set! buffer (bitwise-ior (<< buffer n) b))
        (set! bits-in-buffer (+ bits-in-buffer n)))
      
      (define (set-buffer! n b)
        (set! buffer b)
        (set! bits-in-buffer n))
      
      (define (integer-length n)
        (unless (and (integer? n) (not (negative? n)))
          (error "a non-negative integer was expected, got: " n))
        (if (<= n 1)
            1
            (+ 1 (integer-length (arithmetic-shift n -1)))))
      
      (define (flush-buffer)
        (unless (zero? bits-in-buffer)
          (byte-writer (<< buffer (- 8 bits-in-buffer)))))
      
      (define (write-n-bits n b)
        (when (and (number? n) (not (zero? n))
                   (> (integer-length b) n))
          (error "doh!" (list n b)))
        ; (set! b (low-bits n b))
        (cond
          ((not (positive? n))
           #f)
	  ((zero? bits-in-buffer)	; the bit-buffer is empty
	   (case n
	     ((8)  (byte-writer b))
	     ((16) (byte-writer (>> (bitwise-and #b1111111100000000 b) 8))
                   (byte-writer (bitwise-and #b11111111 b)))
             (else
              (let ([r (remainder n 8)])
                (cond
                  ((zero? r)  (for-each byte-writer
                                        (let loop ([n n] [b b] [l '()])
                                          (if (= n 0)  
                                              l
                                              (loop (- n 8)
                                                    (>> b 8)
                                                    (cons (bitwise-and #b11111111 b) l))))))
                  ((< n 8)  (set-buffer! n b))
                  ((< n 16) (byte-writer (>> b (- n 8)))
                            (set-buffer! (- n 8) (bitwise-and b (>> #b11111111 (- 16 n)))))
                  (else     (let ([bits-to-buffer (remainder n 8)]) ; output all whole bytes, and buffer the rest
                              (write-n-bits (- n bits-to-buffer)
                                            (>> b bits-to-buffer))
                              (set-buffer! bits-to-buffer                
                                           (bitwise-and b (vector-ref #(0 1 3 7 15 31 63 127 255) r))))))))))
          ((< n (- 8 bits-in-buffer))  ; everything goes to the buffer
           (extend-buffer! n b))
          (else                        
           (let ([m (- 8 bits-in-buffer)])
             ;(display (list buffer bits-in-buffer n) (current-error-port))
             ; (flush-output (current-error-port))
             ; the buffer and the initial bits make a byte
             
             (byte-writer (bitwise-ior (<< buffer m)
                                       (>> b (- n m))))
             (empty-buffer!)
             ; write the rest
             (write-n-bits (- n m) (bitwise-xor 
                                    b (<< (>> b (- n m))
                                          (- n m))))))))
      (values write-n-bits flush-buffer)))
  
  
  ;;; TEST
  
  #;
  (define (test)
    (define (naturals n)
      (do ([i 0 (+ i 1)]
           [l '() (cons i l)])
        [(= i n) l]))
    
    ; write the numbers 999 ... 1 to "tmp" and read them again
    
    (with-output-to-file "tmp"
      (lambda ()
        (let-values 
            ([(write flush) (make-bit-writer  write-byte)])
          (for-each (lambda (n)
                      (write n n))
                    (naturals 100))
          (flush)))
      'replace)
    
    (with-input-from-file "tmp"
      (lambda ()
        (let ([r (make-bit-reader read-byte)])
          (for-each (lambda (n)
                      (display (r n))
                      (display " "))
                    (naturals 100))))))
  
  ;;;
  ;;; PARAMETERS
  ;;;
  
  (define current-bit-reader (make-parameter (make-bit-reader read-byte)))
  
  (define-values  (current-bit-writer current-bit-flusher)
    (let-values ([(writer flusher) (make-bit-writer write-byte)])
      (values (make-parameter writer)
              (make-parameter flusher))))
  
  
  )