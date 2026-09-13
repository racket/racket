(module pico '#%kernel

  (#%declare #:cross-phase-persistent)

  (#%provide member memw
             (rename reverse alt-reverse))

  ; ======================================================================
  ; Various functions which are straightforward to write in kernel,
  ; merged into a single cross-phase-persistent module for load time reasons.
  ; ======================================================================
  ;

  ; --------------------------------------------------
  ; member and friends

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
      (member-impl 'memw v ls equal-always?)))


  ; --------------------------------------------------
  ; reverse
  ; (shadows the `reverse` from kernel, which is not as optimizable)

  (define-values (reverse)
    (lambda (l)
      (if (variable-reference-from-unsafe? (#%variable-reference))
          (void)
          (if (list? l)
              (void)
              (raise-argument-error 'reverse "list?" l)))
      (letrec-values ([(loop)
                       (lambda (a l)
                         (if (null? l)
                             a
                             (loop (cons (car l) a) (cdr l))))])
                     (loop null l))))

  ;
  ; --------------------------------------------------
  )
