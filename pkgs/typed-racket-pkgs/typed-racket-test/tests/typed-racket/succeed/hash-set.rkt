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

;; Covariance test
(: foo : (-> (HashTable Symbol Index) Symbol Nonnegative-Fixnum))
(define (foo h k)
  (+ (hash-ref h k)
     (hash-ref h (string->symbol (format "~a-buddy" k)))))
(: bar : (-> Symbol Positive-Byte (IHashTable Symbol Positive-Byte)))
(define (bar k v) (make-immutable-hash (list (cons k v))))

;; This does not work with (make-hash (list (cons 'a 2) (cons 'a-buddy 3)))
(foo (hash-set (h 'a 2) 'a-buddy 3) 'a)

