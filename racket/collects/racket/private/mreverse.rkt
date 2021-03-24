(module mreverse '#%kernel
  (#%declare #:cross-phase-persistent)
  (#%provide (rename mreverse alt-mreverse))

  (define-values (mlist?)
    (λ (l)
      (if (null? l)
          #t
          (if (mpair? l)
              (letrec-values ([(loop)
                               (λ (turtle hare)
                                 (if (null? hare)
                                     #t
                                     (if (eq? hare turtle)
                                         #f
                                         (if (mpair? hare)
                                             ((λ (hare)
                                                (if (null? hare)
                                                    #t
                                                    (if (eq? hare turtle)
                                                        #f
                                                        (if (mpair? hare)
                                                            (loop (mcdr turtle) (mcdr hare))
                                                            #f))))
                                              (mcdr hare))
                                             #f))))])
                (loop l (mcdr l)))
              #f))))

  (define-values (mreverse)
    (lambda (l)
      (if (variable-reference-from-unsafe? (#%variable-reference))
          (void)
          (if (mlist? l)
              (void)
              (raise-argument-error 'mreverse "mlist?" l)))
      (letrec-values ([(loop)
                       (lambda (a l)
                         (if (null? l)
                             a
                             (loop (mcons (mcar l) a) (mcdr l))))])
        (loop null l)))))
