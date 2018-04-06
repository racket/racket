
;;----------------------------------------------------------------------
;; syntax/loc

(module stxloc '#%kernel
  (#%require "qq-and-or.rkt" "stxcase.rkt" "define-et-al.rkt"
             (for-syntax '#%kernel "stxcase.rkt" "sc.rkt"))

  (begin-for-syntax
    (define-values (transform-to-syntax-case**)
      (lambda (stx sc arg-is-stx? expr kws lit-comp s-exp? clauses)
        ((λ (ans) (datum->syntax #'here ans stx))
         (list* 'syntax-case** sc arg-is-stx? expr kws lit-comp s-exp?
                clauses)))))
  
  ;; Like regular syntax-case, but with free-identifier=? replacement
  (-define-syntax syntax-case*
    (lambda (stx)
      (syntax-case** #f #t stx () free-identifier=? #f
	[(sc stxe kl id=? . clause)
         (transform-to-syntax-case** stx #'sc #f #'stxe #'kl #'id=? #f #'clause)])))

  ;; Regular syntax-case
  (-define-syntax syntax-case
    (lambda (stx)
      (syntax-case** #f #t stx () free-identifier=? #f
	[(sc stxe kl . clause)
         (transform-to-syntax-case** stx #'sc #f #'stxe #'kl #'free-identifier=? #f
                                     #'clause)])))

  ;; Like `syntax-case, but on plain datums
  (-define-syntax datum-case
    (lambda (stx)
      (syntax-case** #f #t stx () free-identifier=? #f
	[(sc stxe kl . clause)
	 (transform-to-syntax-case** stx #'sc #f #'stxe #'kl #'eq? #t #'clause)])))

  (-define-syntax quote-syntax/prune
    (lambda (stx)
      (syntax-case** #f #t stx () free-identifier=? #f
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

  (#%provide syntax/loc quote-syntax/prune syntax-case* syntax-case datum-case
             ... _ ~? ~@))
