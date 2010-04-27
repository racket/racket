#lang scheme

(define (parse-kw-args formals actuals source)
  (let loop ([current (for/hash ([arg formals]) (values (car arg) #f))]
             [rest actuals])
    (syntax-case rest ()
      [() (map (λ (arg) (or (hash-ref current (car arg)) (cdr arg))) formals)]
      [(kw . rest)
       (not (keyword? (syntax-e (syntax kw))))
       (raise-syntax-error #f "expected a keyword" source (syntax kw))]
      [(kw arg . rest)
       (keyword? (syntax-e (syntax arg)))
       (raise-syntax-error #f "expected an argument expression" source (syntax arg))]
      [(kw arg . rest)
       (let ([none (gensym)])
         (eq? none (hash-ref current (syntax-e (syntax kw)) none)))
       (raise-syntax-error #f "invalid keyword" source (syntax kw))]
      [(kw arg . rest)
       (hash-ref current (syntax-e (syntax kw)))
       (raise-syntax-error #f "repeated keyword" source (syntax kw))]
      [(kw)
       (raise-syntax-error #f "missing argument expression after keyword" source (syntax kw))]
      [(kw arg . rest)
       (loop (hash-set current (syntax-e (syntax kw)) (syntax arg))
             (syntax rest))]
      [else (raise-syntax-error #f "bad keyword argument syntax" source rest)])))

(provide parse-kw-args)
