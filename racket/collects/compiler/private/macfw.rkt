(module macfw racket/base
  (require "mach-o.rkt"
           racket/string
           (only-in racket/base regexp-quote)
           racket/system)

  (provide update-framework-path
           get-current-framework-path
           update-framework-path/cmdline)

  (define (update-framework-path/cmdline)
    (let ([v (current-command-line-arguments)])
      (update-framework-path (vector-ref v 0)
			     (vector-ref v 1)
			     (equal? (vector-ref v 2) "mred"))))

  (define (update-framework-path fw-path #:as-given? [as-given? #f]
                                 dest
                                 mred?
                                 #:matching [matchings '("Racket")])
    (let ([dest (if (path? dest)
		    (path->string dest)
		    dest)])
      (for-each (lambda (p)
		  (let* ([orig (get-current-framework-path dest p)]
			 [3m (if (and orig (regexp-match #rx"_3m" orig))
				 "_3m"
				 "")]
			 [old-path (or orig
				       (format "~a.framework/Versions/~a~a/~a" p (version) 3m p))]
			 [new-path (if as-given?
                                       (format "~a" fw-path)
                                       (format "~a~a.framework/Versions/~a~a/~a" 
                                               fw-path
                                               p (version) 3m p))])
		    (get/set-dylib-path dest
					(byte-regexp
					 (bytes-append
					  #"^"
					  (string->bytes/utf-8
					   (regexp-quote old-path))
					  #"$"))
					(string->bytes/utf-8 new-path))))
                matchings)))

  (define (get-current-framework-path dest p)
    (let ([v (get/set-dylib-path dest
				 (byte-regexp (string->bytes/utf-8 p))
				 #f)])
      (and (pair? v)
           (bytes->string/utf-8 (car v))))))
