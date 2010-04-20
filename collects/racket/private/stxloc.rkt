
;;----------------------------------------------------------------------
;; syntax/loc

(module stxloc '#%kernel
  (#%require "qq-and-or.rkt" "stxcase.rkt" "define-et-al.rkt"
             (for-syntax '#%kernel "stxcase.rkt" "sc.rkt"))

  ;; Regular syntax-case
  (-define-syntax syntax-case*
    (lambda (stx)
      (syntax-case** #f #t stx () free-identifier=?
	[(sc stxe kl id=? clause ...)
	 (syntax (syntax-case** sc #f stxe kl id=? clause ...))])))

  ;; Regular syntax-case
  (-define-syntax syntax-case
    (lambda (stx)
      (syntax-case** #f #t stx () free-identifier=?
	[(sc stxe kl clause ...)
	 (syntax (syntax-case** sc #f stxe kl free-identifier=? clause ...))])))

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

  (-define-syntax quote-syntax/prune
    (lambda (stx)
      (syntax-case** #f #t stx () free-identifier=?
        [(_ id) 
         (if (symbol? (syntax-e #'id))
             (datum->syntax #'here
                            (list (quote-syntax quote-syntax)
                                  (identifier-prune-lexical-context (syntax id)
                                                                    (list
                                                                     (syntax-e (syntax id))
                                                                     '#%top)))
                            stx
                            #f
                            stx)
             (raise-syntax-error
              #f
              "expected an identifier"
              stx
              #'id))])))

  (#%provide syntax/loc quote-syntax/prune syntax-case* syntax-case ... _))
