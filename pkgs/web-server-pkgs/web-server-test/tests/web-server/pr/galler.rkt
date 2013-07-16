#lang web-server
(require racket/serialize
         rackunit)

(check-true (serializable? (λ (x) x)))
(check-true (serializable? (first (list (λ (x) x)))))
