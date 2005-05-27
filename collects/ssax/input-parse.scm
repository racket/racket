;****************************************************************************
;			Simple Parsing of input
;
; The following simple functions surprisingly often suffice to parse
; an input stream. They either skip, or build and return tokens,
; according to inclusion or delimiting semantics. The list of
; characters to expect, include, or to break at may vary from one
; invocation of a function to another. This allows the functions to
; easily parse even context-sensitive languages.
;
; EOF is generally frowned on, and thrown up upon if encountered.
; Exceptions are mentioned specifically. The list of expected characters 
; (characters to skip until, or break-characters) may include an EOF
; "character", which is to be coded as symbol *eof*
;
; The input stream to parse is specified as a PORT, which is usually
; the last (and optional) argument. It defaults to the current input
; port if omitted.
;
; IMPORT
; This package relies on a function parser-error, which must be defined
; by a user of the package. The function has the following signature:
;	parser-error PORT MESSAGE SPECIALISING-MSG*
; Many procedures of this package call parser-error to report a parsing
; error.  The first argument is a port, which typically points to the
; offending character or its neighborhood. Most of the Scheme systems
; let the user query a PORT for the current position. MESSAGE is the
; description of the error. Other arguments supply more details about
; the problem.
; myenv.scm, myenv-bigloo.scm or a similar prelude is assumed.
; From SRFI-13, string-concatenate-reverse
; If a particular implementation lacks SRFI-13 support, please
; include the file srfi-13-local.scm
;
; $Id: input-parse.scm,v 1.2 2004/11/09 14:11:40 sperber Exp $

;------------------------------------------------------------------------

; -- procedure+: peek-next-char [PORT]
; 	advances to the next character in the PORT and peeks at it.
; 	This function is useful when parsing LR(1)-type languages
; 	(one-char-read-ahead).
;	The optional argument PORT defaults to the current input port.

(define-opt (peek-next-char (optional (port (current-input-port))))
  (read-char port) 
  (peek-char port)) 


;------------------------------------------------------------------------

; -- procedure+: assert-curr-char CHAR-LIST STRING [PORT]
;	Reads a character from the PORT and looks it up
;	in the CHAR-LIST of expected characters
;	If the read character was found among expected, it is returned
;	Otherwise, the procedure writes a nasty message using STRING
;	as a comment, and quits.
;	The optional argument PORT defaults to the current input port.
;
(define-opt (assert-curr-char expected-chars comment
			      (optional (port (current-input-port))))
  (let ((c (read-char port)))
    (if (memv c expected-chars) c
    (parser-error port "Wrong character " c
    	   " (0x" (if (eof-object? c) "*eof*"
    	   	    (number->string (char->integer c) 16)) ") "
    	   comment ". " expected-chars " expected"))))
    	   

; -- procedure+: skip-until CHAR-LIST [PORT]
;	Reads and skips characters from the PORT until one of the break
;	characters is encountered. This break character is returned.
;	The break characters are specified as the CHAR-LIST. This list
;	may include EOF, which is to be coded as a symbol *eof*
;
; -- procedure+: skip-until NUMBER [PORT]
;	Skips the specified NUMBER of characters from the PORT and returns #f
;
;	The optional argument PORT defaults to the current input port.


(define-opt (skip-until arg (optional (port (current-input-port))) )
  (cond
   ((number? arg)		; skip 'arg' characters
      (do ((i arg (dec i)))
      	  ((not (positive? i)) #f)
      	  (if (eof-object? (read-char port))
      	    (parser-error port "Unexpected EOF while skipping "
			 arg " characters"))))
   (else			; skip until break-chars (=arg)
     (let loop ((c (read-char port)))
       (cond
         ((memv c arg) c)
         ((eof-object? c)
           (if (memq '*eof* arg) c
             (parser-error port "Unexpected EOF while skipping until " arg)))
         (else (loop (read-char port))))))))


; -- procedure+: skip-while CHAR-LIST [PORT]
;	Reads characters from the PORT and disregards them,
;	as long as they are mentioned in the CHAR-LIST.
;	The first character (which may be EOF) peeked from the stream
;	that is NOT a member of the CHAR-LIST is returned. This character
;	is left on the stream.
;	The optional argument PORT defaults to the current input port.

(define-opt (skip-while skip-chars (optional (port (current-input-port))) )
  (do ((c (peek-char port) (peek-char port)))
      ((not (memv c skip-chars)) c)
      (read-char port)))
 
; whitespace const

;------------------------------------------------------------------------
;				Stream tokenizers


; -- procedure+: 
;    next-token PREFIX-CHAR-LIST BREAK-CHAR-LIST [COMMENT-STRING] [PORT]
;	skips any number of the prefix characters (members of the
;	PREFIX-CHAR-LIST), if any, and reads the sequence of characters
;	up to (but not including) a break character, one of the
;	BREAK-CHAR-LIST.
;	The string of characters thus read is returned.
;	The break character is left on the input stream
;	The list of break characters may include EOF, which is to be coded as
;	a symbol *eof*. Otherwise, EOF is fatal, generating an error message
;	including a specified COMMENT-STRING (if any)
;
;	The optional argument PORT defaults to the current input port.
;
; Note: since we can't tell offhand how large the token being read is
; going to be, we make a guess, pre-allocate a string, and grow it by
; quanta if necessary. The quantum is always the length of the string
; before it was extended the last time. Thus the algorithm does
; a Fibonacci-type extension, which has been proven optimal.
; Note, explicit port specification in read-char, peek-char helps.

; Procedure: input-parse:init-buffer
; returns an initial buffer for next-token* procedures.
; The input-parse:init-buffer may allocate a new buffer per each invocation:
;	(define (input-parse:init-buffer) (make-string 32))
; Size 32 turns out to be fairly good, on average.
; That policy is good only when a Scheme system is multi-threaded with
; preemptive scheduling, or when a Scheme system supports shared substrings.
; In all the other cases, it's better for input-parse:init-buffer to
; return the same static buffer. next-token* functions return a copy
; (a substring) of accumulated data, so the same buffer can be reused.
; We shouldn't worry about an incoming token being too large:
; next-token will use another chunk automatically. Still, 
; the best size for the static buffer is to allow most of the tokens to fit in.
; Using a static buffer _dramatically_ reduces the amount of produced garbage
; (e.g., during XML parsing).

(define input-parse:init-buffer
  (let ((buffer (make-string 512)))
    (lambda () buffer)))
  

		; See a better version below
(define-opt (next-token-old prefix-skipped-chars break-chars
			(optional (comment "") (port (current-input-port))) )
  (let* ((buffer (input-parse:init-buffer))
	 (curr-buf-len (string-length buffer))
	 (quantum curr-buf-len))
    (let loop ((i 0) (c (skip-while prefix-skipped-chars port)))
      (cond
        ((memv c break-chars) (substring buffer 0 i))
    	((eof-object? c)
    	  (if (memq '*eof* break-chars)
    	    (substring buffer 0 i)		; was EOF expected?
    	    (parser-error port "EOF while reading a token " comment)))
    	(else
    	  (if (>= i curr-buf-len)	; make space for i-th char in buffer
    	    (begin			; -> grow the buffer by the quantum
    	      (set! buffer (string-append buffer (make-string quantum)))
    	      (set! quantum curr-buf-len)
    	      (set! curr-buf-len (string-length buffer))))
    	  (string-set! buffer i c)
    	  (read-char port)			; move to the next char
    	  (loop (inc i) (peek-char port))
    	  )))))


; A better version of next-token, which accumulates the characters
; in chunks, and later on reverse-concatenates them, using
; SRFI-13 if available.
; The overhead of copying characters is only 100% (or even smaller: bulk
; string copying might be well-optimised), compared to the (hypothetical)
; circumstance if we had known the size of the token beforehand.
; For small tokens, the code performs just as above. For large
; tokens, we expect an improvement. Note, the code also has no
; assignments. 
; See next-token-comp.scm

(define-opt (next-token prefix-skipped-chars break-chars
		  (optional (comment "") (port (current-input-port))) )
  (let outer ((buffer (input-parse:init-buffer)) (filled-buffer-l '())
	      (c (skip-while prefix-skipped-chars port)))
    (let ((curr-buf-len (string-length buffer)))
      (let loop ((i 0) (c c))
	(cond
	  ((memv c break-chars)
	    (if (null? filled-buffer-l) (substring buffer 0 i)
	      (string-concatenate-reverse filled-buffer-l buffer i)))
	  ((eof-object? c)
	    (if (memq '*eof* break-chars)	; was EOF expected?
	      (if (null? filled-buffer-l) (substring buffer 0 i)
		(string-concatenate-reverse filled-buffer-l buffer i))
	      (parser-error port "EOF while reading a token " comment)))
	  ((>= i curr-buf-len)
	    (outer (make-string curr-buf-len)
	      (cons buffer filled-buffer-l) c))
	  (else
	    (string-set! buffer i c)
	    (read-char port)			; move to the next char
	    (loop (inc i) (peek-char port))))))))

; -- procedure+: next-token-of INC-CHARSET [PORT]
;	Reads characters from the PORT that belong to the list of characters
;	INC-CHARSET. The reading stops at the first character which is not
;	a member of the set. This character is left on the stream.
;	All the read characters are returned in a string.
;
; -- procedure+: next-token-of PRED [PORT]
;	Reads characters from the PORT for which PRED (a procedure of one
;	argument) returns non-#f. The reading stops at the first character
;	for which PRED returns #f. That character is left on the stream.
;	All the results of evaluating of PRED up to #f are returned in a
;	string.
;
;	PRED is a procedure that takes one argument (a character
;	or the EOF object) and returns a character or #f. The returned
;	character does not have to be the same as the input argument
;	to the PRED. For example,
;	(next-token-of (lambda (c)
;			  (cond ((eof-object? c) #f)
;				((char-alphabetic? c) (char-downcase c))
;				(else #f))))
;	will try to read an alphabetic token from the current
;	input port, and return it in lower case.
; 
;	The optional argument PORT defaults to the current input port.
;
; This procedure is similar to next-token but only it implements
; an inclusion rather than delimiting semantics.

(define-opt (next-token-of incl-list/pred
			   (optional (port (current-input-port))) )
  (let* ((buffer (input-parse:init-buffer))
	 (curr-buf-len (string-length buffer)))
  (if (procedure? incl-list/pred)
    (let outer ((buffer buffer) (filled-buffer-l '()))
      (let loop ((i 0))
	(if (>= i curr-buf-len)		; make sure we have space
	  (outer (make-string curr-buf-len) (cons buffer filled-buffer-l))
	  (let ((c (incl-list/pred (peek-char port))))
	    (if c
	      (begin
		(string-set! buffer i c)
		(read-char port)			; move to the next char
		(loop (inc i)))
	      ; incl-list/pred decided it had had enough
	      (if (null? filled-buffer-l) (substring buffer 0 i)
		(string-concatenate-reverse filled-buffer-l buffer i)))))))

    ; incl-list/pred is a list of allowed characters
    (let outer ((buffer buffer) (filled-buffer-l '()))
      (let loop ((i 0))
	(if (>= i curr-buf-len)		; make sure we have space
	  (outer (make-string curr-buf-len) (cons buffer filled-buffer-l))
	  (let ((c (peek-char port)))
	    (cond
	      ((not (memv c incl-list/pred))
		(if (null? filled-buffer-l) (substring buffer 0 i)
		  (string-concatenate-reverse filled-buffer-l buffer i)))
	      (else
		(string-set! buffer i c)
		(read-char port)			; move to the next char
		(loop (inc i))))))))
    )))


; -- procedure+: read-text-line [PORT]
;	Reads one line of text from the PORT, and returns it as a string.
;	A line is a (possibly empty) sequence of characters terminated
;	by CR, CRLF or LF (or even the end of file).
;	The terminating character (or CRLF combination) is removed from
;	the input stream. The terminating character(s) is not a part
;	of the return string either.
;	If EOF is encountered before any character is read, the return
;	value is EOF.
; 
;	The optional argument PORT defaults to the current input port.

(define *read-line-breaks* (list char-newline char-return '*eof*))

(define-opt (read-text-line (optional (port (current-input-port))) )
  (if (eof-object? (peek-char port)) (peek-char port)
    (let* ((line
             (next-token '() *read-line-breaks*
			 "reading a line" port))
           (c (read-char port)))	; must be either \n or \r or EOF
       (and (eqv? c char-return) (eqv? (peek-char port) #\newline)
         (read-char port))			; skip \n that follows \r
       line)))


; -- procedure+: read-string N [PORT]
;	Reads N characters from the PORT, and  returns them in a string.
;	If EOF is encountered before N characters are read, a shorter string
;	will be returned.
;	If N is not positive, an empty string will be returned.
;	The optional argument PORT defaults to the current input port.

(define-opt (read-string n (optional (port (current-input-port))) )
  (if (not (positive? n)) ""
    (let ((buffer (make-string n)))
      (let loop ((i 0) (c (read-char port)))
        (if (eof-object? c) (substring buffer 0 i)
          (let ((i1 (inc i)))
            (string-set! buffer i c)
            (if (= i1 n) buffer
              (loop i1 (read-char port)))))))))

