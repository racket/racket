(module md5 mzscheme
  
  (provide md5)
  
  ;;; -*- mode: scheme; mode: font-lock -*-
  ;;;
  ;;; Copyright (c) 2002, Jens Axel Søgaard
  ;;; 
  ;;; Permission to copy this software, in whole or in part, to use this
  ;;; software for any lawful purpose, and to redistribute this software
  ;;; is hereby granted.
  ;;;
  ;;; md5.scm  --  Jens Axel Søgaard, 16 oct 2002  
  
  ;;; History
  ; 14-10-2002  /jas
  ;   - Bored. Initial attempt. Done. Well, except for faulty output.
  ; 15-10-2002  /jas
  ;   - It works at last
  ; 16-10-2002  /jas
  ;   - Added R5RS support
  ; 16-02-2003 / lth
  ;   - Removed let-values implementation because Larceny has it already
  ;   - Implemented Larceny versions of many bit primitives (note, 0.52 
  ;     or later required due to bignum bug)
  ;   - Removed most 'personal idiosyncrasies' to give the compiler a fair 
  ;     chance to inline primitives and improve performance some.
  ;     Performance in the interpreter is still really quite awful.
  ;   - Wrapped entire procedure in a big LET to protect the namespace
  ;   - Some cleanup of repeated computations
  ;   - Moved test code to separate file
  ; 17-02-2003 / lth
  ;   - Removed some of the indirection, for a 30% speedup in Larceny's
  ;     interpreter.  Running in the interpreter on my Dell Inspiron 4000
  ;     I get a fingerprint of "Lib/Common/bignums-be.sch" in about 63ms,
  ;     which is slow but adequate.  (The compiled version is not much
  ;     faster -- most time is spent in bignum manipulation, which is
  ;     compiled in either case.  To do this well we must either operate
  ;     on the bignum representation or redo the algorithm to use
  ;     fixnums only.)
  ; 01-12-2003 / lth
  ;   - Reimplemented word arithmetic to use two 16-bit fixnums boxed in
  ;     a cons cell.  In Petit Larceny's interpreter this gives a speedup
  ;     of a factor of almost eight, and in addition this change translates
  ;     well to other Scheme systems that support bit operations on fixnums.
  ;     Only 17-bit (signed) fixnums are required.
  ; 23-12-2003 / jas
  ;   - Trivial port to PLT. Rewrote the word macro to syntax-rules.
  ;     Larceny primitives written as syntax-rules macros exanding
  ;     to their PLT name.
  
  ; 5-5-2005 / Greg Pettyjohn
  ;  - It was failing for strings of length 56 bytes i.e. when the length
  ;    in bits was congruent 448 modulo 512. Changed step 1 to fix this.
  ;    According to RFC 1321, the message should still be padded in this
  ;    case.
  
  ; 23-12-2005 / Jepri
  ; - Mucked around with the insides to get it to read from a port
  ; - Now it accepts a port or a string as input
  ; - Doesn't explode when handed large strings anymore
  ; - Now much slower

  ; 2-10-2006 / Matthew
  ;  - Cleaned up a little
  ;  - Despite comment above, it seems consistently faster
  
  
  ;;; Summary
  ; This is an implementation of the md5 message-digest algorithm
  ; in R5RS Scheme. The algorithm takes an arbitrary string and 
  ; returns a 128-bit "fingerprint". 
  ; The algorithm was invented by Ron Rivest, RSA Security, INC.
  ; Reference:  RFC 1321, <http://www.faqs.org/rfcs/rfc1321.html>
  
  ;;; Contact
  ; Email jensaxel@soegaard.net if you have problems,
  ; suggestions, code for 32 bit arithmetic for your
  ; favorite implementation.
  ; Check <http://www.scheme.dk/md5/> for new versions.
  
  ;;; Technicalities
  ; The algorithm is designed to be efficiently implemented
  ; using 32 bit arithmetic. If your implementation supports
  ; 32 bit arithmetic directly, you should substitute the
  ; portable 32 operations with primitives of your implementation.
  ; See the PLT version below for an example. 
  
  ;;; Word aritmetic (32 bit)
  ; Terminology
  ;    word:  32 bit unsigned integer
  ;    byte:   8 bit unsigned integer
  
  (define md5 'undefined)

  ; (word c) turns into a quoted pair '(hi . lo).  I would have this local to the
  ; let below except Twobit does not allow transformer to be used with let-syntax,
  ; only with define-syntax.
  
  ;(define-syntax word
  ;  (transformer 
  ;    (lambda (expr rename compare)
  ;      (list 'quote (cons (quotient (cadr expr) 65536) (remainder (cadr expr) 65536))))))
  
  (define-syntax word
    (syntax-rules ()
      [(word c)
       (cons (quotient c 65536) (remainder c 65536))]))
  
  (let ()
    
    ;;; PORTING NOTES
    
    ;   logand is bitwise "and" on fixnums
    ;   logior is bitwise "inclusive or" on fixnums
    ;   logxor is bitwise "exclusive or" on fixnums
    ;   lognot is bitwise "not" on fixnums
    ;   lsh    is bitwise left-shift without overflow detection on fixnums
    ;   rshl   is bitwise logical right-shift on fixnums.  Arithmetic 
    ;             right shift (rsha in Larceny) can be used instead of rshl
    ;             in this code
    
    ; PLT versions of the Larceny primitives
    (define-syntax fixnum? (syntax-rules () [(_ n) #t]))
    (define-syntax logand  (syntax-rules () [(_ . more) (bitwise-and . more)]))
    (define-syntax logior  (syntax-rules () [(_ . more) (bitwise-ior . more)]))
    (define-syntax logxor  (syntax-rules () [(_ . more) (bitwise-xor . more)]))
    (define-syntax lognot  (syntax-rules () [(_ . more) (bitwise-not . more)]))
    (define-syntax lsh     (syntax-rules () [(_ n s)    (arithmetic-shift n s)]))
    (define-syntax rshl    (syntax-rules () [(_ n s)    (arithmetic-shift n (- s))]))
    
    ; Words are represented as a cons where the car holds the high 16
    ; bits and the cdr holds the low 16 bits.  Most good Scheme systems
    ; will have fixnums that hold at least 16 bits as well as fast
    ; allocation, so this has a fair chance at beating bignums for
    ; performance.
    
    (define (integer->word i)
      (if (or (and (fixnum? i) (>= i 0))
              (<= 0 i 4294967296))
          (cons (quotient i 65536) (remainder i 65536))
          (error "integer->word: out of range: " i)))
    
    (define (word->integer w)
      (+ (* (car w) 65536) (cdr w)))
    
    (define (word+ a b)
      (let ((t1 (+ (car a) (car b)))
            (t2 (+ (cdr a) (cdr b))))
        (cons (logand (+ t1 (rshl t2 16)) 65535)
              (logand t2 65535))))
    
    (define (word-or a b)
      (cons (logior (car a) (car b))
            (logior (cdr a) (cdr b))))
    
    (define (word-not a)
      (cons (logand (lognot (car a)) 65535) (logand (lognot (cdr a)) 65535)))
    
    (define (word-xor a b)
      (cons (logxor (car a) (car b)) (logxor (cdr a) (cdr b))))
    
    (define (word-and a b)
      (cons (logand (car a) (car b)) (logand (cdr a) (cdr b))))
    
    (define (word<<< a s)
      (define masks
        '#(#x0 #x1 #x3 #x7 #xF #x1F #x3F #x7F #xFF 
               #x1FF #x3FF #x7FF #xFFF #x1FFF #x3FFF #x7FFF #xFFFF))
      (define (rot hi lo s)
        (cons (logior (lsh (logand hi (vector-ref masks (- 16 s))) s)
                      (logand (rshl lo (- 16 s)) (vector-ref masks s)))
              (logior (lsh (logand lo (vector-ref masks (- 16 s))) s)
                      (logand (rshl hi (- 16 s)) (vector-ref masks s)))))
      (cond ((< 0 s 16)
             (rot (car a) (cdr a) s))
            ((< s 32)
             (rot (cdr a) (car a) (- s 16)))
            (else
             (error "word<<<: shift out of range: " s))))
    
    ;; word->bytes : word -> "(list byte byte byte byte)", little endian!
    (define (word->bytes w)
      (list (logand (cdr w) 255)
            (logand (rshl (cdr w) 8) 255)
            (logand (car w) 255)
            (logand (rshl (car w) 8) 255)))
    
    (define (word.4+ a b c d)
      (word+ (word+ (word+ a b) c) d))
    
    (define bitpos 
      '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 
          26 27 28 29 30 31))
    
    (define powers 
      '#(1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536
           131072 262144 524288 1048576 2097152 4194304 8388608 16777216 33554432
           67108864 134217728 268435456 536870912 1073741824 2147483648 
           4294967296))
    
    ;; word->bits : word -> (list (union 0 1))
    (define (word->bits w)
      (let ((w (word->integer w)))
        (define (bit i)  
          (remainder (quotient w (vector-ref powers i)) 2))
        (map bit bitpos)))
    
    ;; bits->integer : (list (union 0 1)) -> integer
    (define (bits->integer bs)
      (apply + (map * bs (map (lambda (i) (vector-ref powers i)) bitpos))))
    
    ;; Bytes and words
    ;; The least significant byte of a word is the first
    
    ;; bytes->word : (list byte*) -> word
    (define (bytes->word bs)
      (define (bs->w akk mul bs)
        (if (null? bs) 
            (integer->word akk)
            (bs->w (+ akk (* (car bs) mul)) (* 256 mul) (cdr bs))))
      (bs->w 0 1 bs))
    
    ;; bytes->words : (list byte) -> (list word)
    (define (bytes->words bytes)
      (define (loop bs l)
        (cond ((null? l)          (list (bytes->word (reverse bs))))
              ((< (length bs) 4)  (loop (cons (car l) bs)  (cdr l)))
              (else               (cons (bytes->word (reverse bs))
                                        (loop '() l)))))
      (if (null? bytes)
          '()
          (loop '() bytes)))
    
    ;; string->bytes : string -> (list byte)
    (define (string->bytes s)
      (bytes->list s))
    ;; Converts a list of words to a vector, just like vector-from-string
    ;; vector-from-list: byte-string -> (vector ...)
    (define vector-from-list list->vector)
    
    ;; Converts a byte string to a more useful vector
    ;; vector-from-string: byte string -> (vector ...)
    (define vector-from-string
      (lambda (l-raw)
        (list->vector (bytes->words (string->bytes l-raw)))))

    (define empty-port (open-input-bytes #""))
    
    ;; List Helper
    ;; block/list : a-port done-n -> (values vector a-port done-n)
    ;; reads 512 bytes from the port, turns them into a vector of 16 32-bit words
    ;; when the port is exhausted it returns #f for the port and
    ;; the last few bytes padded 
    
    (define (block/list a-port done)
      (let* ((l-raw (read-bytes (/ 512 8) a-port)))
        (cond 
          ;; File size was a multiple of 512 bits, or we're doing one more round to
          ;; add the correct padding from the short case
          ((eof-object? l-raw)
           (if (zero? (modulo done (/ 512 8)))
               ;; The file is a multiple of 512 or was 0, so there hasn't been a 
               ;; chance to add the 1-bit pad, so we need to do a full pad
               (values (vector-from-list (step2 (* 8 done)
                                                (step1 (string->bytes #""))))                        
                       #f
                       done)
               ;; We only enter this block when the previous block didn't have
               ;; enough room to fit the 64-bit file length,
               ;; so we just add 448 bits of zeros and then the 64-bit file length (step2)
               (values (vector-from-list (step2 (* 8 done)
                                                (vector->list (make-vector (quotient 448 8) 0)))) 
                       #f
                       done)))
          ;; We read exactly 512 bits, the algorythm proceeds as usual
          ((= (bytes-length l-raw) (/ 512 8))
           (values (vector-from-string l-raw) a-port (+ done (bytes-length l-raw))))
          ;; We read less than 512 bits, so the file has ended.  
          ;; However, we don't have enough room to add the correct trailer,
          ;; so we add what we can, then go for one more round which will
          ;; automatically fall into the (eof-object? case)
          ((> (* 8 (bytes-length l-raw)) 446)
           (let ([done (+ done (bytes-length l-raw))])
             (values (vector-from-list (step2 (* 8 done)
                                              (step1 (string->bytes l-raw))))
                     empty-port
                     done)))
          
          ;; Returning a longer vector than we should, luckily it doesn't matter.
          ;; We read less than 512 bits and there is enough room for the correct trailer.
          ;; Add trailer and bail
          (else
           (let ([done (+ done (bytes-length l-raw))])
             (values (vector-from-list (step2 (* 8 done)
                                              (step1 (string->bytes l-raw))))
                     #f
                     done))))))
    
    ;(step2 (* 8 (bytes-length  str)) 
    ;			 (step1 (string->bytes str)))
    
    
    ;; MD5
    ;; The algorithm consists of five steps.
    ;; All we need to do, is to call them in order.
    ;; md5 : string -> string
    
    (define (md5-computation a-thing)
      (let ((a-port (cond
                      [(bytes? a-thing)
                       (open-input-bytes a-thing)]
                      [(input-port? a-thing)
			a-thing]
                      [else
                       (raise-type-error 'md5
                                         "input-port or bytes"
                                         a-thing)])))
	(step5 (step4 a-port))))
    
    ;; Step 1  -  Append Padding Bits
    ;; The message is padded so the length (in bits) becomes 448 modulo 512.
    ;; We allways append a 1 bit and then append the proper numbber of 0's.
    ;; NB: 448 bits is 14 words and 512 bits is 16 words
    ;; step1 : (list byte) -> (list byte)
    
    (define (step1 message)
      (let* ([z-b-t-a (modulo (- 448 (* 8 (length message))) 512)]
             [zero-bits-to-append (if (zero? z-b-t-a) 512 z-b-t-a)])
        (append message 
                (cons #x80		; The byte containing the 1 bit => one less 
                      ; 0 byte to append 
                      (vector->list
                       (make-vector
                        (quotient (- zero-bits-to-append 1) 8) 0))))))
    
    ;; Step 2  -  Append Length
    ;; A 64 bit representation of the bit length b of the message before
    ;; the padding of step 1 is appended. Lower word first.
    ;; step2 : number (list byte) -> (list word)
    ;;  org-len is the length of the original message in number of bits
    
    (define (step2 original-length padded-message)
      (let* ((b  original-length)
             (lo (remainder b #x100000000))
             (hi (remainder (quotient b #x100000000) #x100000000)))
        (bytes->words 
         (append padded-message 
                 (append (word->bytes (integer->word lo))
                         (word->bytes (integer->word hi)))))))
    
    ;; Step 3  -  Initialize MD Buffer
    ;; These magic constants are used to initialize the loop
    ;; in step 4.
    ;;
    ;;          word A: 01 23 45 67
    ;;          word B: 89 ab cd ef
    ;;          word C: fe dc ba 98
    ;;          word D: 76 54 32 10
    
    ;; Step 4  -  Process Message in 16-Word Blocks
    ;; For each 16 word block, go through a round one to four.
    ;; step4 : (list word) -> "(list word word word word)"
    
    (define (step4 a-port)
      
      (define (loop A B C D a-port done)

        (if (not a-port)
            (list A B C D)
            (let-values (((X b-port done) (block/list a-port done)))
              (if (not X)
                  (list A B C D)
                  (begin
                    (let* ((AA A) 
                           (BB B) 
                           (CC C)
                           (DD D)
                           
                           (A (word+ B (word<<< (word.4+ A (F B C D) (vector-ref X 0) (word 3614090360)) 7)))
                           (D (word+ A (word<<< (word.4+ D (F A B C) (vector-ref X 1) (word 3905402710)) 12)))
                           (C (word+ D (word<<< (word.4+ C (F D A B) (vector-ref X 2) (word 606105819)) 17)))
                           (B (word+ C (word<<< (word.4+ B (F C D A) (vector-ref X 3) (word 3250441966)) 22)))
                           (A (word+ B (word<<< (word.4+ A (F B C D) (vector-ref X 4) (word 4118548399)) 7)))
                           (D (word+ A (word<<< (word.4+ D (F A B C) (vector-ref X 5) (word 1200080426)) 12)))
                           (C (word+ D (word<<< (word.4+ C (F D A B) (vector-ref X 6) (word 2821735955)) 17)))
                           (B (word+ C (word<<< (word.4+ B (F C D A) (vector-ref X 7) (word 4249261313)) 22)))
                           (A (word+ B (word<<< (word.4+ A (F B C D) (vector-ref X 8) (word 1770035416)) 7)))
                           (D (word+ A (word<<< (word.4+ D (F A B C) (vector-ref X 9) (word 2336552879)) 12)))
                           (C (word+ D (word<<< (word.4+ C (F D A B) (vector-ref X 10) (word 4294925233)) 17)))
                           (B (word+ C (word<<< (word.4+ B (F C D A) (vector-ref X 11) (word 2304563134)) 22)))
                           (A (word+ B (word<<< (word.4+ A (F B C D) (vector-ref X 12) (word 1804603682)) 7)))
                           (D (word+ A (word<<< (word.4+ D (F A B C) (vector-ref X 13) (word 4254626195)) 12)))
                           (C (word+ D (word<<< (word.4+ C (F D A B) (vector-ref X 14) (word 2792965006)) 17)))
                           (B (word+ C (word<<< (word.4+ B (F C D A) (vector-ref X 15) (word 1236535329)) 22)))
                           
                           (A (word+ B (word<<< (word.4+ A (G B C D) (vector-ref X 1) (word 4129170786)) 5)))
                           (D (word+ A (word<<< (word.4+ D (G A B C) (vector-ref X 6) (word 3225465664)) 9)))
                           (C (word+ D (word<<< (word.4+ C (G D A B) (vector-ref X 11) (word 643717713)) 14)))
                           (B (word+ C (word<<< (word.4+ B (G C D A) (vector-ref X 0) (word 3921069994)) 20)))
                           (A (word+ B (word<<< (word.4+ A (G B C D) (vector-ref X 5) (word 3593408605)) 5)))
                           (D (word+ A (word<<< (word.4+ D (G A B C) (vector-ref X 10) (word 38016083)) 9)))
                           (C (word+ D (word<<< (word.4+ C (G D A B) (vector-ref X 15) (word 3634488961)) 14)))
                           (B (word+ C (word<<< (word.4+ B (G C D A) (vector-ref X 4) (word 3889429448)) 20)))
                           (A (word+ B (word<<< (word.4+ A (G B C D) (vector-ref X 9) (word 568446438)) 5)))
                           (D (word+ A (word<<< (word.4+ D (G A B C) (vector-ref X 14) (word 3275163606)) 9)))
                           (C (word+ D (word<<< (word.4+ C (G D A B) (vector-ref X 3) (word 4107603335)) 14)))
                           (B (word+ C (word<<< (word.4+ B (G C D A) (vector-ref X 8) (word 1163531501)) 20)))
                           (A (word+ B (word<<< (word.4+ A (G B C D) (vector-ref X 13) (word 2850285829)) 5)))
                           (D (word+ A (word<<< (word.4+ D (G A B C) (vector-ref X 2) (word 4243563512)) 9)))
                           (C (word+ D (word<<< (word.4+ C (G D A B) (vector-ref X 7) (word 1735328473)) 14)))
                           (B (word+ C (word<<< (word.4+ B (G C D A) (vector-ref X 12) (word 2368359562)) 20)))
                           
                           (A (word+ B (word<<< (word.4+ A (H B C D) (vector-ref X 5) (word 4294588738)) 4)))
                           (D (word+ A (word<<< (word.4+ D (H A B C) (vector-ref X 8) (word 2272392833)) 11)))
                           (C (word+ D (word<<< (word.4+ C (H D A B) (vector-ref X 11) (word 1839030562)) 16)))
                           (B (word+ C (word<<< (word.4+ B (H C D A) (vector-ref X 14) (word 4259657740)) 23)))
                           (A (word+ B (word<<< (word.4+ A (H B C D) (vector-ref X 1) (word 2763975236)) 4)))
                           (D (word+ A (word<<< (word.4+ D (H A B C) (vector-ref X 4) (word 1272893353)) 11)))
                           (C (word+ D (word<<< (word.4+ C (H D A B) (vector-ref X 7) (word 4139469664)) 16)))
                           (B (word+ C (word<<< (word.4+ B (H C D A) (vector-ref X 10) (word 3200236656)) 23)))
                           (A (word+ B (word<<< (word.4+ A (H B C D) (vector-ref X 13) (word 681279174)) 4)))
                           (D (word+ A (word<<< (word.4+ D (H A B C) (vector-ref X 0) (word 3936430074)) 11)))
                           (C (word+ D (word<<< (word.4+ C (H D A B) (vector-ref X 3) (word 3572445317)) 16)))
                           (B (word+ C (word<<< (word.4+ B (H C D A) (vector-ref X 6) (word 76029189)) 23)))
                           (A (word+ B (word<<< (word.4+ A (H B C D) (vector-ref X 9) (word 3654602809)) 4)))
                           (D (word+ A (word<<< (word.4+ D (H A B C) (vector-ref X 12) (word 3873151461)) 11)))
                           (C (word+ D (word<<< (word.4+ C (H D A B) (vector-ref X 15) (word 530742520)) 16)))
                           (B (word+ C (word<<< (word.4+ B (H C D A) (vector-ref X 2) (word 3299628645)) 23)))
                           
                           (A (word+ B (word<<< (word.4+ A (II B C D) (vector-ref X 0) (word 4096336452)) 6)))
                           (D (word+ A (word<<< (word.4+ D (II A B C) (vector-ref X 7) (word 1126891415)) 10)))
                           (C (word+ D (word<<< (word.4+ C (II D A B) (vector-ref X 14) (word 2878612391)) 15)))
                           (B (word+ C (word<<< (word.4+ B (II C D A) (vector-ref X 5) (word 4237533241)) 21)))
                           (A (word+ B (word<<< (word.4+ A (II B C D) (vector-ref X 12) (word 1700485571)) 6)))
                           (D (word+ A (word<<< (word.4+ D (II A B C) (vector-ref X 3) (word 2399980690)) 10)))
                           (C (word+ D (word<<< (word.4+ C (II D A B) (vector-ref X 10) (word 4293915773)) 15)))
                           (B (word+ C (word<<< (word.4+ B (II C D A) (vector-ref X 1) (word 2240044497)) 21)))
                           (A (word+ B (word<<< (word.4+ A (II B C D) (vector-ref X 8) (word 1873313359)) 6)))
                           (D (word+ A (word<<< (word.4+ D (II A B C) (vector-ref X 15) (word 4264355552)) 10)))
                           (C (word+ D (word<<< (word.4+ C (II D A B) (vector-ref X 6) (word 2734768916)) 15)))
                           (B (word+ C (word<<< (word.4+ B (II C D A) (vector-ref X 13) (word 1309151649)) 21)))
                           (A (word+ B (word<<< (word.4+ A (II B C D) (vector-ref X 4) (word 4149444226)) 6)))
                           (D (word+ A (word<<< (word.4+ D (II A B C) (vector-ref X 11) (word 3174756917)) 10)))
                           (C (word+ D (word<<< (word.4+ C (II D A B) (vector-ref X 2) (word 718787259)) 15)))
                           (B (word+ C (word<<< (word.4+ B (II C D A) (vector-ref X 9) (word 3951481745)) 21)))
                           
                           (A (word+ A AA)) 
                           (B (word+ B BB))
                           (C (word+ C CC)) 
                           (D (word+ D DD)))
                      
                      (loop A B C D b-port done)))))))
        
      ;; Step 3 :-) (magic constants)
      ;;    (display (format "Message is: ~a~n~n" message))
      (loop (word #x67452301) (word #xefcdab89) (word #x98badcfe) (word #x10325476) a-port 0))
    
    ;; Each round consists of the application of the following
    ;; basic functions. They functions on a word bitwise, as follows.
    ;;          F(X,Y,Z) = XY v not(X) Z  (NB: or can be replaced with + in F)
    ;;          G(X,Y,Z) = XZ v Y not(Z)
    ;;          H(X,Y,Z) = X xor Y xor Z
    ;;          I(X,Y,Z) = Y xor (X v not(Z))
    
    (define (F x y z)
      (word-or (word-and x y) (word-and (word-not x) z)))
    
    (define (G x y z)
      (word-or (word-and x z) (word-and y (word-not z))))
    
    (define (H x y z)
      (word-xor x (word-xor y z)))
    
    (define (II x y z)
      (word-xor y (word-or x (word-not z))))
    
    ;; Step 5  -  Output
    ;; To finish up, we convert the word to hexadecimal string
    ;; - and make sure they end up in order.
    ;; step5 : "(list word word word word)" -> string
    
    (define (step5 l)
      
      
      (define hex #(48 49 50 51 52 53 54 55 56 57 97 98 99 100 101 102))
      
      (define (number->hex n)
        (bytes (vector-ref hex (quotient n 16))
               (vector-ref hex (modulo n 16))))
      
      (apply bytes-append
             (map number->hex
                  (apply append (map word->bytes l)))))
    (set! md5 md5-computation)
    )
  
;(define (md5-test)
;  (if (and (equal? (md5 "")
;                   "d41d8cd98f00b204e9800998ecf8427e")
;           (equal? (md5 "a")
;                   "0cc175b9c0f1b6a831c399e269772661")
;           (equal? (md5 "abc")
;                   "900150983cd24fb0d6963f7d28e17f72")
;           (equal? (md5 "message digest")
;                   "f96b697d7cb7938d525a2f31aaf161d0")
;           (equal? (md5 "abcdefghijklmnopqrstuvwxyz")
;                   "c3fcd3d76192e4007dfb496cca67e13b")
;           (equal? (md5 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
;                   "d174ab98d277d9f5a5611c2c9f419d9f")
;           (equal? (md5 "12345678901234567890123456789012345678901234567890123456789012345678901234567890")
;                   "57edf4a22be3c955ac49da2e2107b67a"))
;      'passed
;      'failed))
;
;(md5-test)
  )
