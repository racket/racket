(module cmdargs racket/base

  (provide split-command-line-args)

  (define (split-command-line-args v)
    (let loop ([v (strip-leading-spaces (strip-trailing-spaces v))])
      (if (string=? v "")
	  null
	  (let-values ([(s v) (let loop ([v v])
				(cond
				 [(string=? v "") (values "" "")]
				 [(regexp-match #rx"^[ \t\r\n]" v) (values "" v)]
				 [(regexp-match-positions #rx"^\"[^\"]*\"" v)
				  => (combine v loop 1)]
				 [(regexp-match-positions #rx"^'[^']*'" v)
				  => (combine v loop 1)]
				 [(regexp-match-positions #rx"^[^ \t\r\n]+" v)
				  => (combine v loop 0)]))])
	    (cons s (loop (strip-leading-spaces v)))))))

  (define (combine v loop inset)
    (lambda (m)
      (let-values ([(rest leftover) (loop (substring v (cdar m)))])
	(values (string-append
		 (substring v (+ (caar m) inset) (- (cdar m) inset)))
		leftover))))

  (define (strip-leading-spaces v)
    (regexp-replace #rx"^[\t \r\n]+" v ""))

  (define (strip-trailing-spaces v)
    (regexp-replace #rx"[\t \r\n]+$" v "")))
