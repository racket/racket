
(module cgi-unit mzscheme
  (require (lib "unitsig.ss")
	   (lib "etc.ss"))

  (require "cgi-sig.ss")

  (provide net:cgi@)
  (define net:cgi@
    (unit/sig net:cgi^
      (import)
      
      ;; type bindings = list ((symbol . string))

      ;; --------------------------------------------------------------------

      ;; Exceptions:

      (define-struct cgi-error ())

      ;; chars : list (char)
      ;; -- gives the suffix which is invalid, not including the `%'

      (define-struct (incomplete-%-suffix cgi-error) (chars))

      ;; char : char
      ;; -- an invalid character in a hex string

      (define-struct (invalid-%-suffix cgi-error) (char))

      ;; --------------------------------------------------------------------

      ;; query-chars->string :
      ;; list (char) -> string

      ;; -- The input is the characters post-processed as per Web specs, which
      ;; is as follows:
      ;; spaces are turned into "+"es and lots of things are turned into %XX,
      ;; where XX are hex digits, eg, %E7 for ~.  The output is a regular
      ;; Scheme string with all the characters converted back.

      (define query-chars->string
	(lambda (chars)
	  (list->string
	   (let loop ((chars chars))
	     (if (null? chars) null
		 (let ((first (car chars))
		       (rest (cdr chars)))
		   (let-values (((this rest)
				 (cond
				  ((char=? first #\+)
				   (values #\space rest))
				  ((char=? first #\%)
				   (if (and (pair? rest)
					    (pair? (cdr rest)))
				       (values
					(integer->char
					 (or (string->number
					      (string
					       (car rest) (cadr rest))
					      16)
					     (raise (make-invalid-%-suffix
						     (if (string->number
							  (string (car rest))
							  16)
							 (cadr rest)
							 (car rest))))))
					(cddr rest))
				       (raise
					(make-incomplete-%-suffix rest))))
				  (else
				   (values first rest)))))
		     (cons this (loop rest)))))))))

      ;; string->html :
      ;; string -> string
      ;; -- the input is raw text, the output is HTML appropriately quoted

      (define string->html
	(lambda (s)
	  (apply string-append
		 (map (lambda (c)
			(case c
			  ((#\<) "&lt;")
			  ((#\>) "&gt;")
			  ((#\&) "&amp;")
			  (else (string c))))
		      (string->list s)))))

      (define default-text-color "#000000")
      (define default-bg-color "#ffffff")
      (define default-link-color "#cc2200")
      (define default-vlink-color "#882200")
      (define default-alink-color "#444444")

      ;; generate-html-output :
      ;; html-string x list (html-string) x ... -> ()

      (define generate-html-output
	(opt-lambda (title body-lines
			   (text-color default-text-color)
			   (bg-color default-bg-color)
			   (link-color default-link-color)
			   (vlink-color default-vlink-color)
			   (alink-color default-alink-color))
	  (let ((sa string-append))
	    (for-each 
	     (lambda (l)
	       (display l) (newline))
	     `("Content-type: text/html"
	       ""
	       "<html>"
	       "<!-- The form was processed, and this document was generated,"
	       "     using the CGI utilities for MzScheme.  For more information"
	       "     on MzScheme, see"
	       "      http://www.plt-scheme.org/software/mzscheme/"
	       "     and for the CGI utilities, contact"
	       "     (sk@cs.brown.edu). -->"
	       
	       "<head>"
	       ,(sa "<title>" title "</title>")
	       "</head>"
	       ""
	       ,(sa "<body bgcolor=\"" bg-color "\" text=\"" text-color "\"")
	       ,(sa "      link=\"" link-color "\"")
	       ,(sa "      vlink=\"" vlink-color "\" alink=\"" alink-color "\">")
	       ""
	       ,@body-lines
	       ""
	       "</body>"
	       "</html>")))))
      
      ;; output-http-headers : -> void
      (define (output-http-headers)
	(printf "Content-type: text/html~a~n~a~n" #\return #\return))
      
      ;; read-until-char :
      ;; iport x char -> list (char) x bool
      ;; -- operates on the default input port; the second value indicates
      ;; whether reading stopped because an EOF was hit (as opposed to the
      ;; delimiter being seen); the delimiter is not part of the result
      
      (define read-until-char
	(lambda (ip delimiter)
	  (let loop ((chars '()))
	    (let ((c (read-char ip)))
	      (cond
	       ((eof-object? c)
		(values (reverse chars) #t))
	       ((char=? c delimiter)
		(values (reverse chars) #f))
	       (else
		(loop (cons c chars))))))))

      ;; read-name+value :
      ;; iport -> (symbol + bool) x (string + bool) x bool

      ;; -- If the first value is false, so is the second, and the third is
      ;; true, indicating EOF was reached without any input seen.  Otherwise,
      ;; the first and second values contain strings and the third is either
      ;; true or false depending on whether the EOF has been reached.  The
      ;; strings are processed to remove the CGI spec "escape"s.

      ;; This code is _slightly_ lax: it allows an input to end in `&'.  It's
      ;; not clear this is legal by the CGI spec, which suggests that the last
      ;; value binding must end in an EOF.  It doesn't look like this matters.
      ;; It would also introduce needless modality and reduce flexibility.

      (define read-name+value
	(lambda (ip)
	  (let-values 
	      (((name eof?)
		(read-until-char ip #\=)))
	    (cond
	     ((and eof? (null? name))
	      (values #f #f #t))
	     (eof?
	      (generate-error-output
	       (list "Server generated malformed input for POST method:"
		     (string-append
		      "No binding for `" (list->string name) "' field."))))
	     (else
	      (let-values (((value eof?)
			    (read-until-char ip #\&)))
		(values (string->symbol (query-chars->string name))
			(query-chars->string value)
			eof?)))))))
      
      ;; get-bindings/post :
      ;; () -> bindings
      
      (define get-bindings/post
	(lambda ()
	  (let-values (((name value eof?)
			(read-name+value
			 (current-input-port))))
	    (cond
	     ((and eof? (not name))
	      null)
	     ((and eof? name)
	      (list (cons name value)))
	     (else
	      (cons (cons name value)
		    (get-bindings/post)))))))

      ;; get-bindings/get :
      ;; () -> bindings

      (define get-bindings/get
	(lambda ()
	  (let ((p (open-input-string
		    (getenv "QUERY_STRING"))))
	    (let loop ()
	      (let-values (((name value eof?)
			    (read-name+value p)))
		(cond
		 ((and eof? (not name))
		  null)
		 ((and eof? name)
		  (list (cons name value)))
		 (else
		  (cons (cons name value)
			(loop)))))))))

      ;; get-bindings :
      ;; () -> bindings

      (define get-bindings
	(lambda ()
	  (if (string=? (get-cgi-method) "POST")
	      (get-bindings/post)
	      (get-bindings/get))))

      ;; generate-error-output :
      ;; list (html-string) -> <exit>

      (define generate-error-output
	(lambda (error-message-lines)
	  (generate-html-output "Internal Error"
				error-message-lines)
	  (exit)))

      ;; bindings-as-html :
      ;; bindings -> list (html-string)
      ;; -- formats name-value bindings as HTML appropriate for displaying

      (define bindings-as-html
	(lambda (bindings)
	  `("<code>"
	    ,@(map
	       (lambda (bind)
		 (string-append
		  (symbol->string (car bind))
		  "&nbsp;--&gt;&nbsp;"
		  (cdr bind)
		  "<br>"))
	       bindings)
	    "</code>")))

      ;; extract-bindings :
      ;; (string + symbol) x bindings -> list (string)

      ;; -- Extracts the bindings associated with a given name.  The semantics
      ;; of forms states that a CHECKBOX may use the same NAME field multiple
      ;; times.  Hence, a list of strings is returned.  Note that the result
      ;; may be the empty list.

      (define extract-bindings
	(lambda (field-name bindings)
	  (let ((field-name (if (symbol? field-name) field-name
				(string->symbol field-name))))
	    (let loop ((found null) (bindings bindings))
	      (if (null? bindings)
		  found
		  (if (equal? field-name (caar bindings))
		      (loop (cons (cdar bindings) found) (cdr bindings))
		      (loop found (cdr bindings))))))))

      ;; extract-binding/single : 
      ;; (string + symbol) x bindings -> string
      ;; -- used in cases where only one binding is supposed to occur

      (define extract-binding/single
	(lambda (field-name bindings)
	  (let ((field-name (if (symbol? field-name) field-name
				(string->symbol field-name))))
	    (let ((result (extract-bindings field-name bindings)))
	      (cond
	       ((null? result)
		(generate-error-output
		 `(,(string-append "No binding for field `"
				   (if (symbol? field-name)
				       (symbol->string field-name)
				       field-name)
				   "' in <p>")
		   ,@(bindings-as-html bindings))))
	       ((null? (cdr result))
		(car result))
	       (else
		(generate-error-output
		 `(,(string-append "Multiple bindings for field `"
				   (if (symbol? field-name)
				       (symbol->string field-name)
				       field-name)
				   "' where only one was expected in <p>")
		   ,@(bindings-as-html bindings)))))))))

      ;; get-cgi-method :
      ;; () -> string
      ;; -- string is either GET or POST (though future extension is possible)

      (define get-cgi-method
	(lambda ()
	  (getenv "REQUEST_METHOD")))

      ;; generate-link-text :
      ;; string x html-string -> html-string

      (define generate-link-text
	(lambda (url anchor-text)
	  (string-append "<a href=\"" url "\">" anchor-text "</a>")))

      ;; ====================================================================

      )))

