#lang typed/racket

(: h : (MHashTable Symbol Integer))
(define h (make-hash '((a . 0))))
(hash-set! h 'b 1)

(hash-ref! h 'c (λ _ 3))

(: f : (All (A B) (case-> (-> (IHashTable A B) A B B)
                          (-> (MHashTable A B) A B Void))))
(define (f h x v)
  (if (immutable? h)
      (hash-ref h x)
      (hash-set! h x v)))
