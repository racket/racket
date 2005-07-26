
(module reader mzscheme
  (provide r6rs-readtable
	   (rename r6rs-read read)
	   (rename r6rs-read-syntax read-syntax))

  ;; for raise-read-[eof-]error:
  (require (lib "readerr.ss" "syntax"))

  (define hex-digits (string->list "0123456789abcdefABCDEF"))
  (define standard-delimiters (string->list ";',`()[]{}"))

  ;; hex-value : char -> int
  (define (hex-value ch)
    (cond
     [(char-numeric? ch)
      (- (char->integer ch) 48)]
     [(memv ch '(#\a #\b #\c #\d #\e #\f))
      (- (char->integer ch) 87)]
     [else
      (- (char->integer ch) 55)]))

  ;; read-delimited-string : char input-port .... -> string
  ;;  Reads a string or symbol, given the closing character
  (define (read-delimited-string closer-ch port
				 what src line col pos)
    ;; raise-bad-eof
    ;;  Reports an unexpected EOF in a string/symbol
    (define (raise-bad-eof len)
      (raise-read-eof-error 
       (format "unexpected end-of-file in ~a" what) 
       src line col pos len))

    ;; to-hex : char int -> int
    ;;  Checks input and gets it's value as a hex digit
    (define (to-hex ch len)
      (unless (memv ch hex-digits)
	(if (eof-object? ch)
	    (raise-bad-eof len)
	    (raise-read-error 
	     (format "expected a hex digit for ~a, found: ~e" what ch)
	     src line col pos len)))
      (hex-value ch))

    ;; loop to read string/symbol characters; track the length for error reporting
    (let loop ([chars null][len 1])
      (let ([ch (read-char port)])
	(cond
	 ;; eof
	 [(eof-object? ch) (raise-bad-eof len)]
	 ;; closing quote or bar
	 [(char=? ch closer-ch) (list->string (reverse chars))]
	 ;; escape
	 [(char=? ch #\\)
	  (let ([ch (read-char port)])
	    (cond
	     ;; eof after escape
	     [(eof-object? ch) (raise-bad-eof (add1 len))]
	     ;; newline escape
	     [(char=? #\newline ch)
	      ;; Eat whitespace until we find a newline...
	      (let w-loop ([len (+ len 1)])
		(let ([ch (peek-char port)])
		  (cond
		   [(eof-object? ch) (raise-bad-eof len)]
		   [(and (char-whitespace? ch)
			 (not (char=? #\newline ch))) 
		    (read-char port)
		    (w-loop (+ len 1))]
		   [else
		    (loop chars len)])))]
	     ;; space escape
	     [(char=? #\space ch)
	      (loop (cons #\space chars) (+ len 2))]
	     ;; 2-digit hex escape
	     [(char=? #\x ch)
	      (let* ([ch1 (to-hex (read-char port) (+ len 2))]
		     [ch2 (to-hex (read-char port) (+ len 3))])
		(loop (cons (integer->char (+ (* ch1 16) ch2)) 
			    chars)
		      (+ len 3)))]
	     ;; 4-digit hex escape
	     [(char=? #\u ch)
	      (let* ([ch1 (to-hex (read-char port) (+ len 2))]
		     [ch2 (to-hex (read-char port) (+ len 3))]
		     [ch3 (to-hex (read-char port) (+ len 4))]
		     [ch4 (to-hex (read-char port) (+ len 5))])
		(let ([v (+ (* ch1 4096) (* ch2 256) (* ch3 16) ch4)])
		  (when (<= #xD8FF v #xDFFF)
		    (raise-read-error 
		     (format "out-of-range character for ~a: \\u~a~a~a~a" 
			     what ch1 ch2 ch3 ch4)
		     src line col pos (+ len 5)))
		  (loop (cons (integer->char v) chars) 
			(+ len 5))))]
	     ;; 8-digit hex escape
	     [(char=? #\U ch)
	      (let* ([ch1 (to-hex (read-char port) (+ len 2))]
		     [ch2 (to-hex (read-char port) (+ len 3))]
		     [ch3 (to-hex (read-char port) (+ len 4))]
		     [ch4 (to-hex (read-char port) (+ len 5))]
		     [ch5 (to-hex (read-char port) (+ len 6))]
		     [ch6 (to-hex (read-char port) (+ len 7))]
		     [ch7 (to-hex (read-char port) (+ len 8))]
		     [ch8 (to-hex (read-char port) (+ len 9))])
		(let ([v (+ (* ch1 268435456) (* ch2 16777216) (* ch3 1048576) (* ch4 65536)
			    (* ch5 4096) (* ch6 256) (* ch7 16) ch8)])
		  (when (or (> v #x10FFFF)
			    (<= #xD8FF v #xDFFF))
		    (raise-read-error 
		     (format "out-of-range character for ~a: \\U~a~a~a~a~a~a~a~a" 
			     what ch1 ch2 ch3 ch4 ch5 ch6 ch7 ch8)
		     src line col pos (+ len 9)))
		  (loop (cons (integer->char v) chars)
			(+ len 9))))]
	     ;; other escapes
	     [else (let ([v (case ch
			      [(#\a) 7]
			      [(#\b) 8]
			      [(#\t) 9]
			      [(#\n) 10]
			      [(#\v) 11]
			      [(#\f) 12]
			      [(#\r) 13]
			      [(#\") 34]
			      [(#\\) 92]
			      [(#\|) 124]
			      ;; not a valid escape!
			      [else
			       (raise-read-error 
				(format "illegal escape for ~a: \\~a" what ch)
				src line col pos (+ len 2))])])
		     (loop (cons (integer->char v) chars) (+ len 2)))]))]
	 ;; other character
	 [else (loop (cons ch chars) (+ len 1))]))))

  ;; read-quoted-symbol
  ;;  Reader macro for |
  (define (read-quoted-symbol ch port src line col pos)
    (string->symbol (read-delimited-string #\| port
					    "symbol" src line col pos)))
  
  ;; read-quoted-string
  ;;  Reader macro for "
  (define (read-quoted-string ch port src line col pos)
    (read-delimited-string #\" port 
			    "string" src line col pos))

  ;; read-character
  ;;  Reader macro for characters
  (define (read-character ch port src line col pos)

    ;; make-char-const : list-of-char len -> char
    ;;  Checks whether the character sequence names a char,
    ;;  and either reports and error or returns the character
    (define (make-char-const chars len)
      (let ([chars (reverse chars)])
	(if (null? (cdr chars))
	    ;; simple case: single character
	    (car chars)
	    ;; multi-character name:
	    (let ([name (list->string chars)])
	      ;; raise-bad-char
	      ;;  When it's not a valid character
	      (define (raise-bad-char detail)
		(raise-read-error 
		 (format "bad character constant~a: #\\~a" detail name)
		 src line col pos len))

	      ;; hex-char : int -> char
	      ;;  Checks whether chars has n hex digits, and
	      ;;  produces the character if so
	      (define (hex-char n)
		(unless (= (+ n 1) (length chars))
		  (raise-bad-char (format " (expected ~a hex digits after #\\~a) "
					  n
					  (car chars))))
		(for-each (lambda (c) 
			    (unless (memv c hex-digits)
			      (raise-bad-char (format " (expected hex digit, found ~a) " c))))
			  (cdr chars))
		(let loop ([n 0][chars (cdr chars)])
		  (if (null? chars)
		      (begin
			(when (or (> n #x10FFFF)
				  (<= #xD8FF n #xDFFF))
			  (raise-read-error 
			   (format "out-of-range character: #\\~a" name)
			   src line col pos (+ len 9)))
			(integer->char n))
		      (loop (+ (* n 16) (hex-value (car chars)))
			    (cdr chars)))))

	      ;; Check for standard names or hex, and report an error if not
	      (case (string->symbol name)
		[(nul) (integer->char 0)]
		[(alarm) (integer->char 7)]
		[(backspace) (integer->char 8)]
		[(tab) (integer->char 9)]
		[(newline linefeed) (integer->char 10)]
		[(vtab) (integer->char 11)]
		[(page) (integer->char 12)]
		[(return) (integer->char 13)]
		[(esc) (integer->char 27)]
		[(space) (integer->char 32)]
		[(delete) (integer->char 127)]
		[else
		 ;; Hex?
		 (case (car chars)
		   [(#\x)
		    (hex-char 2)]
		   [(#\u)
		    (hex-char 4)]
		   [(#\U)
		    (hex-char 8)]
		   [else
		    (raise-bad-char "")])])))))
    
    ;; read the leading character:
    (let ([ch (read-char port)])
      (when (eof-object? ch)
	(raise-read-eof-error "unexpected end-of-file after #\\"
			      src line col pos 2))
      ;; loop until delimiter:
      (let loop ([len 3][chars (list ch)])
	(let ([ch (peek-char port)])
	  (if (eof-object? ch)
	      ;; eof is a delimiter
	      (make-char-const chars len)
	      ;; otherwise, consult the current readtable to find delimiters
	      ;; in case someone extends r6rs-readtable:
	      (let-values ([(kind proc dispatch-proc) 
			    (readtable-mapping (current-readtable) ch)])
		(cond
		 [(eq? kind 'terminating-macro) 
		  ;; a terminating macro is a delimiter by definition
		  (make-char-const chars len)]
		 [(or (char-whitespace? ch)
		      (member ch standard-delimiters))
		  ;; something mapped to one of the standard delimiters is
		  ;; a delimiter
		  (make-char-const chars len)]
		 [else 
		  ;; otherwise, it's not a delimiter
		  (read-char port)
		  (loop (add1 len) (cons ch chars))])))))))

  (define (reject-backslash ch port src line col pos)
    (raise-read-error 
     "illegal character in input: \\"
     src line col pos 1))

  ;; r6rs-readtable
  ;;  Extends MzScheme's default reader to handle quoted symbols,
  ;;   strings, and characters:
  (define r6rs-readtable
    (make-readtable #f
		    ;; New syntax:
		    #\| 'terminating-macro read-quoted-symbol
		    #\" 'terminating-macro read-quoted-string
		    #\\ 'dispatch-macro read-character
		    ;; Disable \ symbol escape:
		    #\\ 'terminating-macro reject-backslash))
		    

  ;; r6rs-read
  ;;  Like the normal read, but uses r6rs-readtable
  (define r6rs-read
    (case-lambda
     [() (r6rs-read (current-input-port))]
     [(input) (parameterize ([current-readtable r6rs-readtable])
		(read input))]))

  ;; r6rs-read-syntax
  ;;  Like the normal read-syntax, but uses r6rs-readtable
  (define r6rs-read-syntax
    (case-lambda
     [() (r6rs-read-syntax (object-name (current-input-port)) (current-input-port))]
     [(src-v) (r6rs-read-syntax src-v (current-input-port))]
     [(src-v input) (parameterize ([current-readtable r6rs-readtable])
		      (read-syntax src-v input))])))
