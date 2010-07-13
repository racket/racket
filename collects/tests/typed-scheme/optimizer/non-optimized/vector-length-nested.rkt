(module vector-length typed/scheme 
  (require racket/unsafe/ops)
  (vector-length
   (vector-ref
    (ann (vector (vector 1 2) 2 3)
         (Vector (Vectorof Integer) Integer Integer))
    0)))
