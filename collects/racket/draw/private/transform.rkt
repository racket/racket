#lang racket/base

(provide matrix-vector?
         transformation-vector?
         transformation-vector->immutable)

(define (matrix-vector? m)
  (and (vector? m)
       (= 6 (vector-length m))
       (for/and ([e (in-vector m)])
         (real? e))))

(define (transformation-vector? v)
  (and (vector? v)
       (= 6 (vector-length v))
       (matrix-vector? (vector-ref v 0))
       (real? (vector-ref v 1))
       (real? (vector-ref v 2))
       (real? (vector-ref v 3))
       (real? (vector-ref v 4))
       (real? (vector-ref v 5))))

(define (transformation-vector->immutable v)
  (if (and (immutable? v)
           (immutable? (vector-ref v 0)))
      v
      (vector-immutable
       (vector->immutable-vector (vector-ref v 0))
       (vector-ref v 1)
       (vector-ref v 2)
       (vector-ref v 3)
       (vector-ref v 4)
       (vector-ref v 5))))
