#lang scheme

(require mzlib/list net/head)

(define msgs
  (sort (filter (lambda (x) (regexp-match #rx"^[0-9]*$" (path->string x))) (directory-list))
        (lambda (a b) (< (string->number (path->string a))
			 (string->number (path->string b))))))

(define mailbox
  (let loop ([msgs msgs][p 1])
    (if (null? msgs)
	null
	(let ([msg (car msgs)]
	      [rest (loop (cdr msgs) (add1 p))])
	  (let ([header (with-input-from-file msg
		     (lambda () (read-string (file-size msg))))])
	    (cons (list
		   (string->number (path->string msg))
		   p
		   (file-exists? (format "~abody" msg))
		   (extract-field "From" header)
		   (extract-field "Subject" header)
		   null
		   #f)
		  rest))))))

(with-output-to-file "mailbox" (lambda () (write mailbox) (newline)) 
                     #:exists 'truncate)
