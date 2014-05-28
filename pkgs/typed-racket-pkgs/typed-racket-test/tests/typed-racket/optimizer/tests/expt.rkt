#;#;
#<<END
TR missed opt: expt.rkt 13:5 (expt -2.0 0.5) -- unexpected complex type
TR missed opt: expt.rkt 6:13 (expt (sin 0.25) 1.0) -- unexpected complex type
TR opt: expt.rkt 12:5 (expt 2.0 3.0) -- binary float
TR opt: expt.rkt 6:19 (sin 0.25) -- unary float
END
#<<END
"8.00000"
"0.00000"

END
#lang typed/racket
#reader tests/typed-racket/optimizer/reset-port

;; PR 12526
;; result of expt was Float-Complex, and shouldn't have been
;; this let to incorrect optimization
(define (crash)
  (real-part (expt (sin 0.25) 1.0)))

;; to avoid machine-specific precision issues
(: out : Number -> String)
(define (out v) (real->decimal-string (real-part v) 5))

(out (expt 2.0 3.0))
(out (expt -2.0 0.5)) ; not a valid optimization, returns a complex
