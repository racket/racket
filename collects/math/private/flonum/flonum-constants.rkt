#lang typed/racket/base

(require racket/flonum
         "flonum-bits.rkt")

(provide -max.0 -max-subnormal.0 -min.0 +min.0 +max-subnormal.0 +max.0
         epsilon.0)

(define -max.0 (flnext -inf.0))
(define -min.0 (flprev 0.0))
(define +min.0 (flnext 0.0))
(define +max.0 (flprev +inf.0))

(define +max-subnormal.0 (ordinal->flonum #xfffffffffffff))
(define -max-subnormal.0 (fl- 0.0 +max-subnormal.0))

;; The smallest flonum that can be added to 1.0 to get a result != 1.0
(define epsilon.0 (flulp 1.0))
