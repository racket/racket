#;#;
#<<END
TR missed opt: expt.rkt 14:13 (expt (sin 0.25) 1.0) -- unexpected complex type
TR opt: expt.rkt 14:19 (sin 0.25) -- unary float
END
""

#lang typed/racket

;; PR 12526
;; result of expt was Float-Complex, and shouldn't have been
;; this let to incorrect optimization
(define (crash)
  (real-part (expt (sin 0.25) 1.0)))
