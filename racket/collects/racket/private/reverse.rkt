(module reverse '#%kernel
  (#%provide (rename reverse alt-reverse))

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
                     (loop null l)))))
