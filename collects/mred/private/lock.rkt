(module lock mzscheme
  (require racket/draw/lock)
  (provide as-entry
           as-exit
           entry-point
           (protect mk-param))

  (define-syntax mk-param
    (lambda (stx)
      (syntax-case stx ()
	[(_ val filter check force-redraw)
	 (syntax
	  (case-lambda
	   [() val]
	   [(v) (check v)
	    (let ([v2 (filter v)])
	      (unless (eq? v2 val)
		(set! val v2)
		(force-redraw)))]))]))))
