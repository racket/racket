; Module header is generated automatically
#cs(module input-parse mzscheme
(require mzlib/defmacro)
(require "common.ss")
(require "myenv.ss")
(require "parse-error.ss")

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
;
; $Id: input-parse.scm,v 1.1.1.1 2001/07/11 19:33:43 oleg Exp $

;(declare 			; Gambit-compiler optimization options
; (block)
; (standard-bindings)
; (extended-bindings)		; Needed for #!optional arguments, DSSSL-style
; (fixnum)			; optional, keyword and rest arguments
;)
;(include "myenv.scm") ; include target dependent stuff

;------------------------------------------------------------------------
;		     Preparation and tuning section

; This package is heavily used. Therefore, we take time to tune it in,
; in particular for Gambit.


; Concise and efficient definition of a function that takes one or two
; optional arguments, e.g.,
;
; (define-opt (foo arg1 arg2 (optional (arg3 init3) (arg4 init4))) body)
;
; define-opt is identical to a regular define, with one exception: the
; last argument may have a form
;	(optional (binding init) ... )

(cond-expand
 ((or bigloo gambit)

    ; For Gambit and Bigloo, which support DSSSL extended lambdas,
    ; define-opt like the one in the example above is re-written into
    ; (define-opt (foo arg1 arg2 #!optional (arg3 init3) (arg4 init4)) body)
  (define-macro (define-opt bindings body . body-rest)
    (let* ((rev-bindings (reverse bindings))
	   (opt-bindings
	    (and (pair? rev-bindings) (pair? (car rev-bindings))
		 (eq? 'optional (caar rev-bindings))
		 (cdar rev-bindings))))
      (if opt-bindings
	`(define ,(append (reverse
			   (cons (with-input-from-string "#!optional" read)
				 (cdr rev-bindings)))
			  opt-bindings)
	   ,body ,@body-rest)
	`(define ,bindings ,body ,@body-rest))))
  )
 (plt  ; DL: borrowed from "define-opt.scm"

  (define-syntax define-opt
   (syntax-rules (optional)
    ((define-opt (name . bindings) . bodies)
      (define-opt "seek-optional" bindings () ((name . bindings) . bodies)))

    ((define-opt "seek-optional" ((optional . _opt-bindings))
       (reqd ...) ((name . _bindings) . _bodies))
      (define (name reqd ... . _rest)
	(letrec-syntax
	  ((handle-opts
	     (syntax-rules ()
	       ((_ rest bodies (var init))
		 (let ((var (if (null? rest) init
			      (if (null? (cdr rest)) (car rest)
				(error "extra rest" rest)))))
		   . bodies))
	       ((_ rest bodies var) (handle-opts rest bodies (var #f)))
	       ((_ rest bodies (var init) . other-vars)
		 (let ((var (if (null? rest) init (car rest)))
		       (new-rest (if (null? rest) '() (cdr rest))))
		   (handle-opts new-rest bodies . other-vars)))
	       ((_ rest bodies var . other-vars)
		 (handle-opts rest bodies (var #f) . other-vars))
	       ((_ rest bodies)		; no optional args, unlikely
		 (let ((_ (or (null? rest) (error "extra rest" rest))))
		   . bodies)))))
	  (handle-opts _rest _bodies . _opt-bindings))))

    ((define-opt "seek-optional" (x . rest) (reqd ...) form)
      (define-opt "seek-optional" rest (reqd ... x) form))

    ((define-opt "seek-optional" not-a-pair reqd form)
      (define . form))			; No optional found, regular define

    ((define-opt name body)		; Just the definition for 'name',
      (define name body))		; for compatibilibility with define
  ))
 )
 (else

    ; For Scheme systems without DSSSL extensions, we rewrite the definition
    ; of foo of the example above into the following:
    ;	(define (foo arg1 arg2 . rest)
    ;      (let* ((arg3 (if (null? rest) init3 (car rest)))
    ;	          (arg4 (if (or (null? rest) (null? (cdr rest))) init4
    ;		            (cadr rest)))
    ;        body))
    ; We won't handle more than two optional arguments

  (define-macro define-opt (lambda (bindings body . body-rest)
    (let* ((rev-bindings (reverse bindings))
	   (opt-bindings
	    (and (pair? rev-bindings) (pair? (car rev-bindings))
		 (eq? 'optional (caar rev-bindings))
		 (cdar rev-bindings))))
      (cond
       ((not opt-bindings)		; No optional arguments
	`(define ,bindings ,body ,@body-rest))
       ((null? opt-bindings)
	`(define ,bindings ,body ,@body-rest))
       ((or (null? (cdr opt-bindings)) (null? (cddr opt-bindings)))
	(let* ((rest (gensym))		; One or two optional args
	       (first-opt (car opt-bindings))
	       (second-opt (and (pair? (cdr opt-bindings))
				(cadr opt-bindings))))
	  `(define ,(let loop ((bindings bindings))
		      (if (null? (cdr bindings)) rest
			  (cons (car bindings) (loop (cdr bindings)))))
	     (let* ((,(car first-opt) (if (null? ,rest)
					  ,(cadr first-opt)
					  (car ,rest)))
		    ,@(if second-opt
			  `((,(car second-opt) 
			     (if (or (null? ,rest) (null? (cdr ,rest)))
				 ,(cadr second-opt)
				 (cadr ,rest))))
			  '()))
	       ,body ,@body-rest))))
       (else
	'(error "At most two options are supported"))))))
  ))

(cond-expand
 (gambit
      ; The following macro makes a macro that turns (read-char port)
      ; into (##read-char port). We can't enter such a macro-converter
      ; directly as readers of SCM and Bigloo, for ones, don't like
      ; identifiers with two leading # characters
   (define-macro (gambitize clause)
     `(define-macro ,clause
	,(list 'quasiquote
	    (cons
	     (string->symbol (string-append "##"
					    (symbol->string (car clause))))
	     (map (lambda (id) (list 'unquote id)) (cdr clause))))))
   (gambitize (read-char port))
   (gambitize (peek-char port))
   (gambitize (eof-object? port))
   ;(gambitize (string-append a b))
   )
 (else #t))



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
    (if (memq c expected-chars) c
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
      (do ((i arg (-- i)))
      	  ((<= i 0) #f)
      	  (if (eof-object? (read-char port))
      	    (parser-error port "Unexpected EOF while skipping "
			 arg " characters"))))
   (else			; skip until break-chars (=arg)
     (let loop ((c (read-char port)))
       (cond
         ((memv c arg) c)
         ((eof-object? c)
           (if (memv '*eof* arg) c
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

; Procedure input-parse:init-buffer
; returns an initial buffer for next-token* procedures.
; The input-parse:init-buffer may allocate a new buffer per each invocation:
;	(define (input-parse:init-buffer) (make-string 32))
; Size 32 turns out to be fairly good, on average.
; That policy is good only when a Scheme system is multi-threaded with
; preemptive scheduling, or when a Scheme system supports shared substrings.
; In all the other cases, it's better for input-parse:init-buffer to
; return the same static buffer. next-token* functions return a copy
; (a substring) of accumulated data, so the same buffer can be reused.
; We shouldn't worry about new token being too large: next-token will use
; a larger buffer automatically. Still, the best size for the static buffer
; is to allow most of the tokens to fit in.
; Using a static buffer _dramatically_ reduces the amount of produced garbage
; (e.g., during XML parsing).
(define input-parse:init-buffer
  (let ((buffer (make-string 512)))
    (lambda () buffer)))

(define-opt (next-token prefix-skipped-chars break-chars
			(optional (comment "") (port (current-input-port))) )
  (let* ((buffer (input-parse:init-buffer))
	 (curr-buf-len (string-length buffer)) (quantum 16))
    (let loop ((i 0) (c (skip-while prefix-skipped-chars port)))
      (cond
        ((memq c break-chars) (substring buffer 0 i))
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
    	  (loop (++ i) (peek-char port))
    	  )))))
    	

; Another version of next-token, accumulating characters in a list rather
; than in a string buffer. I heard that it tends to work faster.
; In reality, it works just as fast as the string buffer version above,
; but it allocates 50% more memory and thus has to run garbage collection
; 50% as many times. See next-token-comp.scm

(define-opt (next-token-list-based prefix-skipped-chars break-chars
		  (optional (comment "") (port (current-input-port))) )
  (let* ((first-char (skip-while prefix-skipped-chars port))
         (accum-chars (cons first-char '())))
    (cond 
      ((eof-object? first-char)
        (if (memq '*eof* break-chars) ""
          (parser-error port "EOF while skipping before reading token "
		       comment)))
      ((memq first-char break-chars) "")
      (else
        (read-char port)		; consume the first-char
        (let loop ((tail accum-chars) (c (peek-char port)))
          (cond
            ((memq c break-chars) (list->string (reverse tail)))
            ((eof-object? c)
              (if (memq '*eof* break-chars)
                (list->string (reverse tail))		; was EOF expected?
                (parser-error port "EOF while reading a token " comment)))
            (else
              (read-char port)		; move to the next char
              (loop (list* c tail) (peek-char port))
        )))))))


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
; Note: since we can't tell offhand how large the token being read is
; going to be, we make a guess, pre-allocate a string, and grow it by
; quanta if necessary. The quantum is always the length of the string
; before it was extended the last time. Thus the algorithm does
; a Fibonacci-type extension, which has been proven optimal.
;
; This procedure is similar to next-token but only it implements
; an inclusion rather than delimiting semantics.

(define-opt (next-token-of incl-list/pred
			   (optional (port (current-input-port))) )
  (let* ((buffer (input-parse:init-buffer))
	 (curr-buf-len (string-length buffer)) (quantum 16))
  (if (procedure? incl-list/pred)
    (let loop ((i 0) (c (peek-char port)))
      (cond
        ((incl-list/pred c) =>
          (lambda (c)
            (if (>= i curr-buf-len)	; make space for i-th char in buffer
              (begin			; -> grow the buffer by the quantum
                (set! buffer (string-append buffer (make-string quantum)))
                (set! quantum curr-buf-len)
                (set! curr-buf-len (string-length buffer))))
            (string-set! buffer i c)
            (read-char port)			; move to the next char
            (loop (++ i) (peek-char port))))
        (else (substring buffer 0 i))))
			; incl-list/pred is a list of allowed characters
    (let loop ((i 0) (c (peek-char port)))
      (cond
        ((not (memq c incl-list/pred)) (substring buffer 0 i))
    	(else
    	  (if (>= i curr-buf-len)	; make space for i-th char in buffer
    	    (begin			; -> grow the buffer by the quantum
    	      (set! buffer (string-append buffer (make-string quantum)))
    	      (set! quantum curr-buf-len)
    	      (set! curr-buf-len (string-length buffer))))
    	  (string-set! buffer i c)
    	  (read-char port)			; move to the next char
    	  (loop (++ i) (peek-char port))
    	  ))))))

(cond-expand
 (plt
  #t  ; DL: already available in PLT
 )
 (else

; -- procedure+: read-line [PORT]
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

(define-opt (read-line (optional (port (current-input-port))) )
  (if (eof-object? (peek-char port)) (peek-char port)
    (let* ((line
             (next-token '() '(#\newline #\return *eof*)
			 "reading a line" port))
           (c (read-char port)))	; must be either \n or \r or EOF
       (and (eq? c #\return) (eq? (peek-char port) #\newline)
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
          (let ((i1 (++ i)))
            (string-set! buffer i c)
            (if (= i1 n) buffer
              (loop i1 (read-char port)))))))))

))

(provide (all-defined)))
