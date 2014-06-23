#lang typed/racket

;; Invariance test for mutable hashes
(: foo : (-> (HashTable Symbol Index) Symbol Nonnegative-Fixnum))
(define (foo h k)
  (+ (hash-ref h k)
     (hash-ref h (string->symbol (format "~a-buddy" k)))))
(define bar (make-hash (list (cons 'a 2) (cons 'a-buddy 3))))
(foo bar 'a)

