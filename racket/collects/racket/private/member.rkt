(module member '#%kernel
  (#%require "cond.rkt" "qq-and-or.rkt")
  (#%provide member)

  ;; helper for member error cases
  (define-values (bad-list)
    (λ (who orig-l)
      (raise-mismatch-error who "not a proper list: " orig-l)))

  (define-values (member)
    (letrec-values ([(member)
                     (lambda (v orig-l eql?)
                       (let loop ([ls orig-l] [turtle orig-l])
                         (cond
                           [(null? ls) #f]
                           [(not (pair? ls))
                            (bad-list 'member orig-l)]
                           [(eql? v (car ls)) ls]
                           [else
                            (let ([ls (cdr ls)])
                              (cond
                                [(null? ls) #f]
                                [(or (not (pair? ls))
                                     (eq? ls turtle))
                                 (bad-list 'member orig-l)]
                                [(eql? v (car ls)) ls]
                                [else (loop (cdr ls) (cdr turtle))]))])))])
      (case-lambda
        [(v ls) (member v ls equal?)]
        [(v ls eql?)
         (if (and (procedure? eql?)
                  (procedure-arity-includes? eql? 2))
             (void)
             (raise-argument-error
              'member
              "(procedure-arity-includes/c 2)"
              eq?))
         (member v ls eql?)]))))
