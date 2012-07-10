#lang racket

(require math/private/inverse-hyperbolic
         (only-in math/constants +epsilon.0)
         plot
         unstable/flonum)

(define asinh-cutoff1 (sqrt (sqrt +epsilon.0)))
(define asinh-cutoff2 (/ 1.0 (sqrt +epsilon.0)))
(define asinh-cutoff3 0.5)
(define asinh-cutoff4 (sqrt +epsilon.0))

(plot (function asinh -5 5))

(plot (function asinh (flstep 0.0 -50) (flstep 0.0 50)))
(plot (function asinh (flstep asinh-cutoff1 -50) (flstep asinh-cutoff1 50))
      #:width 500)
(plot (function asinh (flstep asinh-cutoff2 -50) (flstep asinh-cutoff2 50))
      #:width 500)
(plot (function asinh (flstep asinh-cutoff3 -50) (flstep asinh-cutoff3 50))
      #:width 500)
(plot (function asinh (flstep asinh-cutoff4 -50) (flstep asinh-cutoff4 50))
      #:width 500)

(define acosh-cutoff1 (+ 1.0 (sqrt +epsilon.0)))
(define acosh-cutoff2 (/ 1.0 (sqrt +epsilon.0)))
(define acosh-cutoff3 1.5)

(plot (function acosh 1 11))

(plot (function acosh 1.0 (flstep 1.0 100)))
(plot (function acosh (flstep acosh-cutoff1 -50) (flstep acosh-cutoff1 50))
      #:width 500)
(plot (function acosh (flstep acosh-cutoff2 -50) (flstep acosh-cutoff2 50))
      #:width 500)
(plot (function acosh (flstep acosh-cutoff3 -50) (flstep acosh-cutoff3 50))
      #:width 500)

(define atanh-cutoff1 (sqrt (sqrt +epsilon.0)))
(define atanh-cutoff2 0.5)
(define atanh-cutoff3 (sqrt +epsilon.0))

(plot (function atanh -1 1))

(plot (function atanh (flstep 0.0 -50) (flstep 0.0 50)))
(plot (function atanh (flstep 1.0 -100) 1.0))
(plot (function atanh (flstep atanh-cutoff1 -50) (flstep atanh-cutoff1 50))
      #:width 500)
(plot (function atanh (flstep atanh-cutoff2 -50) (flstep atanh-cutoff2 50))
      #:width 500)
(plot (function atanh (flstep atanh-cutoff3 -50) (flstep atanh-cutoff3 50))
      #:width 500)
