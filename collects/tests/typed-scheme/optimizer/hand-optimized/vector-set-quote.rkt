(module vector-set-quote typed/scheme #:optimize
  (require racket/unsafe/ops)
  (unsafe-vector*-set! (ann (vector '(1 2)) (Vector Any))
                       0
                       '(+ 1.0 2.0))) ; we should not optimize under quote
