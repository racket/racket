#lang scheme/base
(provide make-immutable-hash/list-init
         hash-set hash-ref hash-map)

(define (make-immutable-hash/list-init [init '()])
  (make-immutable-hash
   (map (λ (x) (cons (car x) (cadr x)))
        init)))
