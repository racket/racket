; Module header is generated automatically
#cs(module mime mzscheme
(require "common.ss")
(require "myenv.ss")
(require "input-parse.ss")
(require (lib "string.ss" "srfi/13"))

;	Handling of MIME Entities and their parts
;
; According to RFC 2045, "Multipurpose Internet Mail Extensions (MIME)
;  Part One, Format of Internet Message Bodies",
;
; "The term 'entity', refers specifically to the MIME-defined header
; fields and contents of either a message or one of the parts in the
; body of a multipart entity.  The specification of such entities is
; the essence of MIME.  Since the contents of an entity are often
; called the 'body', it makes sense to speak about the body of an
; entity.  Any sort of field may be present in the header of an entity,
; but only those fields whose names begin with "content-" actually have
; any MIME-related meaning."
;
; Specifically, the MIME standard (RFC 2045) defines the following
; MIME-related headers (header fields)
;	Content-type
;	Content-Transfer-Encoding
;	Content-ID
;	Content-Description
;
; Generally we leave content interpretation and processing to a
; user-supplied handler. However, if the MIME entity turns out to
; be composite (multipart), this file provides code to disassemble
; it into separate discrete parts, and have them handled, in turn.
; Composite entities are distinguished by their Content-type (media type)
; of multipart/mixed, multipart/alternative, multipart/parallel,
; multipart/digest, or some other multipart type.
; At present, all of them are handled the same way.


; HTTP character types
; Section "2.2 Basic Rules" of the HTTP 1.1 document

(define (http-token-char? x)
  (or (char-alphabetic? x)
      (char-numeric? x)
      (string-index "!#$%&'*+-.^_`|~" x)))


;------------------------------------------------------------------------
;		Parse the Content-type string
;
; Given a Content-Type string:
;	media-type [; attr=value]*
; return the list of associations (attr . value)
; where attr is a symbol and value is a string.
; The media-type is returned as an association with the type
; '=mime-type'
; See Sections 2.2 and 3.6 of rfc2616 (HTTP/1.1) for syntax of the
; Content-Type string

(define (MIME:parse-content-type ctype-str)
  (call-with-input-string ctype-str
    (lambda (port)
      (let loop ((attrs 
		  (list (cons '=mime-type 
			      (next-token '() '(#\space #\; *eof* #\tab) 
					  "reading media type" port)))))
	(skip-while '(#\space #\tab) port)
	(if (not (eqv? #\; (read-char port)))	; must be EOF
	    attrs				; return the attributes
	    (let ((attr-name
		   (string->symbol (next-token '(#\space #\tab) '(#\=)
					 "reading attr-name" port))))
	      (read-char port)		; skip the #\= separator
	      ; loading attr-value, which is (section 2.2 of HTTP1.1):
	      ;   attr-value = token | quoted-string
	      ;   quoted-string  = ( <"> *(qdtext | quoted-pair ) <"> )
	      ;   qdtext         = <any TEXT except <">>
	      ;   quoted-pair    = "\" CHAR
	      (cond 
	       ((eq? #\" (peek-char port))	; we're reading a quoted-string
		(read-char port)		; skip the opening quote
		(let qsloop ((old-fragments '()))
		  (let ((fragments
			 (cons
			  (next-token '() '(#\" #\\)
				      "reading quoted-string" port)
			  old-fragments)))
		    (if (char=? #\" (read-char port))
			(loop		; finished reading the quoted-string
			 (cons
			  (cons
			   attr-name
			   (apply string-append (reverse fragments)))
			  attrs))
		      ; we've read a backslash. Read the next char literally
			(qsloop (cons (string (read-char port)) fragments))
			))))
	       (else			; reading token
		(assert (char? (peek-char port))
			(http-token-char? (peek-char port)))
		(loop
		 (cons
		  (cons attr-name
			(next-token '() '(#\space #\; *eof* #\tab) 
					  "reading token" port))
		  attrs))))
	      ))))))

; read-headers port
; The procedure reads MIME headers from the port.
; The port will be positioned after the empty line that
; separates the headers.
; Later on, make a separate procedure: read-a-header

(define MIME:read-headers
    (let ()
      (define (read-new-header http-port resp-headers)
	(let ((c (peek-char http-port)))
	  (cond
	   ((eqv? c #\return)		; An empty line, the end of headers
	    (if (eqv? #\newline (peek-next-char http-port))
		(read-char http-port))	; skip the following \n if any
	    resp-headers)
	   ((eqv? c #\newline)	  ; #\return should have been appeared before
	    (read-char http-port) ; but not all servers are compliant
	    resp-headers)
	   ((char-alphabetic? c)  ; beginning of the new header
	    (let* ((header-name
		    (string->symbol
		     (string-upcase
		      (next-token '() '(#\: #\space #\tab *eof*) ""
				  http-port))))
		   (delim (skip-while '(#\space #\tab) http-port))
		   (header-value
		    (if (eqv? delim #\:)
			(begin (read-char http-port)
			       (skip-while '(#\space #\tab) http-port)
			       (read-line http-port))
			#f)))
	      (if (string? header-value)
		  (check-cont http-port resp-headers
			      header-name header-value)
		  (error "BAD-HEADER: " resp-headers))))
	   (else
	    (error "BAD-HEADER: " resp-headers)))))

      ; check to see if the value of the header continues on the next line
      (define (check-cont http-port resp-headers
			  header-name header-value)
	(let ((c (peek-char http-port)))
	  (cond
	   ((or (eqv? c #\space) (eqv? c #\tab))	; it continues
	    (let ((cont-value (read-line http-port)))
	      (check-cont http-port resp-headers
		    header-name (string-append header-value cont-value))))
	   (else
	    (read-new-header http-port
			     (cons (cons header-name header-value)
				   resp-headers))))))
      (lambda (http-port)
	(read-new-header http-port '()))
      ))


(provide (all-defined)))
