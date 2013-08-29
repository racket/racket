#lang racket/base

(define filename "stypes.h")

(define lines
  (with-input-from-file filename
    (lambda ()
      (let loop ()
	(let ([l (read-line)])
	  (if (eof-object? l)
	      null
	      (cons l (loop))))))))

(define n 0)

(with-output-to-file filename
  #:exists 'truncate
  (lambda ()
    (for-each
     (lambda (l)
       (cond
	[(regexp-match #rx"^( +[a-z_A-Z][a-z_A-Z0-9]*,) *(?:/[*] [0-9]* [*]/)? *$" l)
	 => (lambda (m)
	      (let ([s (cadr m)])
		(printf "~a~a\n" 
			s
			(format "~a/* ~a */"
				(make-string (max 0 (- 40 (string-length s))) #\space)
				n)))
	      (set! n (add1 n)))]
	[(regexp-match #rx"^ +[a-zA-Z_][a-z_A-Z0-9]*," l)
	 (set! n (add1 n))
	 (printf "~a\n" l)]
	[else
	 (printf "~a\n" l)]))
     lines)))
