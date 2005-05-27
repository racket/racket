
(require (lib "list.ss")
	 (lib "head.ss" "net"))

(define msgs (quicksort
	      (filter (lambda (x) (regexp-match "^[0-9]*$" x)) (directory-list))
	      (lambda (a b) (< (string->number a) (string->number b)))))

(define mailbox
  (let loop ([msgs msgs][p 1])
    (if (null? msgs)
	null
	(let ([msg (car msgs)]
	      [rest (loop (cdr msgs) (add1 p))])
	  (let ([header (with-input-from-file msg
		     (lambda () (read-string (file-size msg))))])
	    (cons (list
		   (string->number msg)
		   p
		   (file-exists? (format "~abody" msg))
		   (extract-field "From" header)
		   (extract-field "Subject" header)
		   null
		   #f)
		  rest))))))

(with-output-to-file "mailbox" (lambda () (write mailbox) (newline)) 'truncate)
