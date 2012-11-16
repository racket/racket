#lang typed/racket/base

(require racket/flonum
         "flonum-bits.rkt")

(provide -max.0 -max-subnormal.0 -min.0 +min.0 +max-subnormal.0 +max.0
         epsilon.0)

(define -max.0 (assert (flnext -inf.0) negative?))
(define -min.0 (assert (flprev 0.0) negative?))
(define +min.0 (assert (flnext 0.0) positive?))
(define +max.0 (assert (flprev +inf.0) positive?))

(define +max-subnormal.0 (assert (ordinal->flonum #xfffffffffffff) positive?))
(define -max-subnormal.0 (- +max-subnormal.0))

;; The smallest flonum that can be added to 1.0 to get a result != 1.0
(define epsilon.0 (assert (flulp 1.0) positive?))
