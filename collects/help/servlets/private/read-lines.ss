(module read-lines mzscheme

  (require (lib "etc.ss")
	   (lib "pregexp.ss")
	   "util.ss")	

  (provide read-lines)

  (define read-lines
    (opt-lambda (file caption [offset #f])
      (template caption (get-the-lines file offset))))
  
  (define (semi-flatten lst)
    (if (null? lst) 
	'()
	(cons (caar lst)
	      (cons (cadar lst)
		    (semi-flatten (cdr lst))))))

  (define temp-anchor `(A ((NAME "temp")) ""))

  (define (spacify s)
    (if (and (string? s) (string=? s "")) 
	" " ; to appease IE
	s))

  (define (template caption lines) 
    `(TABLE ((CELLPADDING "0")
	     (CELLSPACING "0"))
	    (B ,(color-with "blue" caption))
	    (P)
	    (PRE ((STYLE "font-family:monospace"))
            ; use <BR>'s instead of newlines, for Opera
            ; don't put in a <BR> for the temp-anchor, which wasn't a line in the source
		 ,@(semi-flatten 
		    (map (lambda (ln) 
			   (if (eq? ln temp-anchor)
			       `(,ln "")
			       `(,(spacify ln) (BR)))) lines)))))
    
  (define eoregexp
    "($|\\s|(\\.(\\s|$))|>)")

  (define trailing-regexp
    (pregexp "[\\s>)(\"]"))

  (define url-regexp-base
    (string-append "://([^\\s]*)" eoregexp))

  (define (make-url-regexp ty)
    (pregexp 
     (string-append 
      ty
      url-regexp-base)))

  (define http-regexp (make-url-regexp "http"))
  (define cheap-http-regexp (regexp "http://"))
  (define (http-format url)
    `(A ((HREF ,url)) ,url))
  (define ftp-regexp (make-url-regexp "ftp"))
  (define cheap-ftp-regexp (regexp "ftp://"))
  (define ftp-format http-format) 

  (define email-regexp
    (let ([chars "[^\\s)(<>\"']"])
      (pregexp (string-append chars "+" "@" chars "{3,}"))))
  (define cheap-email-regexp (regexp "@"))
  (define (email-format addr)
    `(A ((HREF ,(string-append "mailto:" addr))) ,addr))

  (define (rtrim s)
    (let* ([presult (pregexp-replace* trailing-regexp s "")]
	   [plen (string-length presult)]
	   [qlen (sub1 plen)])
      (if (and (> qlen 0)
	       (char=? (string-ref presult qlen)
		       #\.))
	  (substring presult 0 qlen)
	  presult)))

  (define (process-for-urls line)
    (let loop ([built-line line])
      (let ([curr-len (string-length built-line)])
	(let-values 
	 ([(raw-indices formatter)
	   (let regexp-loop ([regexps (list http-regexp
                                            ftp-regexp
                                            email-regexp)]
                             [cheap-regexps 
                              (list cheap-http-regexp
                                    cheap-ftp-regexp
                                    cheap-email-regexp)]
                             [formats (list http-format
                                            ftp-format
                                            email-format)])
	     (if (null? regexps)
		 (values #f #f)
		 (let* ([curr-regexp (car regexps)]
			[curr-cheap-regexp (car cheap-regexps)]
			[curr-formatter (car formats)]
			[match-indices
			 (and (regexp-match-positions
			       curr-cheap-regexp built-line)
			      (pregexp-match-positions 
			       curr-regexp built-line))])
		   (if match-indices
		       (values match-indices curr-formatter)
		       (regexp-loop (cdr regexps) (cdr cheap-regexps)
				    (cdr formats))))))])
	 (if raw-indices
	     (let* ([indices (car raw-indices)]
		    [string-start (car indices)]
		    [string-end (cdr indices)]
		    [raw-item 
		     (substring built-line 
				string-start string-end)]
		    [raw-item-len (string-length raw-item)]
		    [item (rtrim raw-item)]
		    [item-len (string-length item)])
		`(TT
		  ,(substring built-line 0 string-start)
		  ,(formatter item)
		  ,(substring raw-item ; text removed by rtrim
			      item-len
			      raw-item-len)
		  ,(loop (substring built-line string-end 
				    curr-len))))
	     built-line)))))

  (define (process-for-keywords line)
    (let ([len (string-length line)])
      (if (and (> len 3)
	       (char=? (string-ref line 0) #\>))
          (let* ([rest-of-line (substring line 1 len)]
                 [port (open-input-string rest-of-line)]
                 [dist
                  (with-handlers ([exn:fail:read? (lambda (x) #f)])
                    (read port)
                    (let-values ([(_1 _2 pos) (port-next-location port)])
                      pos))])
            (if dist
                `(div (b ">" ,(color-highlight (substring line 1 dist)))
                      ,(substring line dist len))
                line))
          #f)))

  ; format line for doc.txt files
  (define (process-doc-line line)
    (let ([key-result (process-for-keywords line)])
      (if key-result
	  key-result
	  (process-for-urls line))))

  (define (get-the-lines file offset)
    (let* ([port (open-input-file file 'text)]
	   [doc-txt? (let ([len (string-length file)])
		       (string=? (substring file (- len 7) len)
				 "doc.txt"))]
	   [process-line
	    (if doc-txt?
		process-doc-line
		(lambda (x) x))]
	   [lines (let loop ([lines '()])	
		    (let ([line (read-line port)])
		      (if (eof-object? line)
			  (begin
			    (close-input-port port)
			    (reverse lines))
			  (loop (cons line lines)))))])
      (if offset
	  (let loop ([lines lines]
		     [count 0])
	    (if (null? lines)
		'()
		(let ([len (add1 (string-length (car lines)))])
	          ; add1 because newline in source omitted
		  (if (>= count offset)
		      (cons temp-anchor
			    (if doc-txt?
				(map process-doc-line lines)
				lines))
		      (cons (process-line (car lines))
			    (loop (cdr lines)
				  (+ count len)))))))
	  (map process-line lines)))))
