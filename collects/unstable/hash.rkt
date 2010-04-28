#lang racket/base

(provide hash-union)

;; map map (key val val -> val) -> map
(define (hash-union h1 h2 f)
  (for/fold ([h* h1])
    ([(k v2) h2])
    (let* ([v1 (hash-ref h1 k #f)]
           [new-val (if v1
                        (f k v1 v2)
                        v2)])      
    (hash-set h* k new-val))))
