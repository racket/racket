(module member '#%kernel
  (#%require "cond.rkt" "qq-and-or.rkt"
             (for-syntax '#%kernel "qq-and-or.rkt"))
  (#%provide memq memv member)

  ;; helper for memq/v/ber error cases
  (define-values (bad-list)
    (λ (who orig-l)
      (raise-mismatch-error who "not a proper list: " orig-l)))

  (define-values (memq memv member)
    (let-values ()
      ;; Create the mem functions
      (define-syntaxes (mk mk-member)
        (values
         (λ (stx)
           (define-values (forms) (syntax-e stx))
           (define-values (id eq?)
             (values (syntax-e (cadr forms))
                     (syntax-e (caddr forms))))
           (datum->syntax
            stx
            `(let-values ([(,id)
                           (lambda (v orig-l)
                             (let loop ([ls orig-l])
                               (cond
                                [(null? ls) #f]
                                [(not (pair? ls))
                                 (bad-list ',id orig-l)]
                                [(,eq? v (car ls)) ls]
                                [else (loop (cdr ls))])))])
                                 ,id)))
         ;; Create the `member` function that takes an extra argument
         ;; Uses `mk` to construct the body
         (λ (stx)
           (define-values (forms) (syntax-e stx))
           (define-values (id) (syntax-e (cadr forms)))
           (datum->syntax
            stx
            `(let* ([default (mk member equal?)]
                    [,id (case-lambda
                           ([v orig-l] (default v orig-l))
                           ([v orig-l eq?]
                            (if (and (procedure? eq?)
                                     (procedure-arity-includes? eq? 2))
                                (void)
                                (raise-argument-error
                                 'member
                                 "(procedure-arity-includes/c 2)"
                                 eq?))
                            ((mk member eq?) v orig-l)))])
               ,id)))))
      (values (mk memq eq?)
              (mk memv eqv?)
              ;; Note that this uses `mk-member`
              (mk-member member)))))

