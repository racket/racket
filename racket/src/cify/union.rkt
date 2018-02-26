#lang racket/base

(provide hash-directed-union
         hash-union)

(define (hash-directed-union a b)
  (for/fold ([a a]) ([(k v) (in-hash b)])
    (hash-set a k v)))

(define (hash-union a b)
  (cond
    [((hash-count a) . < . (hash-count b))
     (hash-directed-union b a)]
    [else
     (hash-directed-union a b)]))
