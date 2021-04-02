(module print-value-columns "pre-base.rkt"
  (#%provide print-value-columns)
  
  (define print-value-columns
    (make-parameter +inf.0
                    (lambda (c)
                      (if (or  (eqv? c +inf.0)
                               (and (exact-integer? c)
                                    ; somewhat arbitrary value, enough for "(list"
                                    (> c 5)))
                          c
                          (raise-argument-error 'print-value-columns "(or/c +inf.0 (and/c exact-integer? (>/c 5)))" c)))
                    'print-value-columns)))

