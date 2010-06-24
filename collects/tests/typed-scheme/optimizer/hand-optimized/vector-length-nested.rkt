(module vector-length typed/scheme #:optimize
  (require racket/unsafe/ops)
  (unsafe-vector*-length
   (unsafe-vector*-ref
    (ann (vector (vector 1 2) 2 3)
         (Vector (Vectorof Integer) Integer Integer))
    0)))
