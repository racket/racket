
;;----------------------------------------------------------------------
;; syntax/loc

(module stxloc '#%kernel
  (#%require "qq-and-or.ss" "stxcase.ss" "define-et-al.ss"
             (for-syntax '#%kernel "stxcase.ss" "sc.ss"))

  ;; Regular syntax-case
  (-define-syntax syntax-case*
    (lambda (stx)
      (syntax-case** #f #t stx () free-identifier=?
	[(_ stxe kl id=? clause ...)
	 (syntax (syntax-case** _ #f stxe kl id=? clause ...))])))

  ;; Regular syntax-case
  (-define-syntax syntax-case
    (lambda (stx)
      (syntax-case** #f #t stx () free-identifier=?
	[(_ stxe kl clause ...)
	 (syntax (syntax-case** _ #f stxe kl free-identifier=? clause ...))])))

  (-define (relocate loc stx)
    (if (or (syntax-source loc)
            (syntax-position loc))
        (datum->syntax stx
                       (syntax-e stx)
                       loc
                       #f
                       stx)
	stx))

  ;; Like syntax, but also takes a syntax object
  ;; that supplies a source location for the
  ;; resulting syntax object.
  (-define-syntax syntax/loc
    (lambda (stx)
      (syntax-case** #f #t stx () free-identifier=?
	[(_ loc pattern)
	 (if (if (symbol? (syntax-e #'pattern))
		 (syntax-pattern-variable? (syntax-local-value #'pattern (lambda () #f)))
		 #f)
	     (syntax (syntax pattern))
	     (syntax (relocate loc (syntax pattern))))])))

  (#%provide syntax/loc syntax-case* syntax-case ... _))
