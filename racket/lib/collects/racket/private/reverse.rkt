(module reverse '#%kernel
  (#%provide alt-reverse)

  (define-values (alt-reverse)
    (if (eval-jit-enabled)
        (let-values ([(reverse)
                      (lambda (l)
                        (if (list? l) 
                            (void)
                            (raise-argument-error 'reverse "list?" l))
                        (letrec-values ([(loop)
                                         (lambda (a l)
                                           (if (null? l)
                                               a
                                               (loop (cons (car l) a) (cdr l))))])
                          (loop null l)))])
          reverse)
        reverse)))


