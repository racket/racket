(module language '#%kernel
  (#%provide current-interaction-info)
  (define-values (current-interaction-info)
    (make-parameter #f
                    (lambda (v)
                      (if (if (not v)
                              #t
                              (if (vector? v)
                                  (if (= 3 (vector-length v))
                                      (if (module-path? (vector-ref v 0))
                                          (symbol? (vector-ref v 1))
                                          #f)
                                      #f)
                                  #f))
                          v
                          (raise-argument-error 'current-interaction-info-module-path
                                                "(or/c #f (vector/c module-path? symbol? any/c))"
                                                v))))))
