#lang racket/base
(require rackunit
         web-server/lang/labels)
(provide labels-tests)

(define l1 (make-labeling #"foo"))
(define l2 (make-labeling #"foo"))
(define l3 (make-labeling #"bar"))
(define l4 (make-labeling #"baz"))

(define labels-tests
  (test-suite
   "Module Labeling"
   
   (test-case
    "The same program produces the same labeling"
    (check-eqv? (l1) (l2))
    (check-eqv? (l1) (l2)))
   
   (test-case
    "Different programs produce different labelings"
    (check-false (eqv? (l3) (l4))))))
