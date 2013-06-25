#;
(
TR missed opt: expt.rkt 13:13 (expt (sin 0.25) 1.0) -- unexpected complex type
TR opt: expt.rkt 13:19 (sin 0.25) -- unary float
)

#lang typed/racket

;; PR 12526
;; result of expt was Float-Complex, and shouldn't have been
;; this let to incorrect optimization
(define (crash)
  (real-part (expt (sin 0.25) 1.0)))
