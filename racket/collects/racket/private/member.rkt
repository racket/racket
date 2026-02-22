(module member '#%kernel
  (#%provide member memw)

  ;; helper for member error cases
  (define-values (bad-list)
    (λ (who orig-l)
      (raise-arguments-error who "not a proper list"
                             "in" orig-l)))

  (define-values (member-impl)
    (lambda (who v orig-l eql?)
      (define-values (loop)
        (lambda (ls turtle)
          (if (null? ls)
              #f
              (if (not (pair? ls))
                  (bad-list who orig-l)
                  (if (eql? v (car ls))
                      ls
                      (let-values ([(ls) (cdr ls)])
                        (if (null? ls)
                            #f
                            (if (if (not (pair? ls)) #t (eq? ls turtle))
                                (bad-list who orig-l)
                                (if (eql? v (car ls))
                                    ls
                                    (loop (cdr ls) (cdr turtle)))))))))))
      (loop orig-l orig-l)))

  (define-values (member)
    (case-lambda
        [(v ls)
         (member-impl 'member v ls equal?)]
        [(v ls eql?)
         (if (if (procedure? eql?)
                 (procedure-arity-includes? eql? 2)
                 #f)
             (void)
             (raise-argument-error 'member
                                   "(procedure-arity-includes/c 2)"
                                   eql?))
         (member-impl 'member v ls eql?)]))
         
  (define-values (memw)
    (lambda (v ls)
      (member-impl 'memw v ls equal-always?))))
