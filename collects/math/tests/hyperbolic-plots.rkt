#lang racket/base

(require math/private/hyperbolic
         (only-in math/constants +epsilon.0)
         plot
         unstable/flonum)

(define sinh-cutoff1 (expt 2.0 -28))
(define sinh-cutoff2 1.0)
(define sinh-cutoff3 22.0)
(define sinh-cutoff4 (log +max.0))
(define sinh-max 710.475860073943)

(plot (function sinh -5 5))

(plot (function sinh (flstep 0.0 -50) (flstep 0.0 50)))
(plot (function sinh (flstep sinh-cutoff1 -50) (flstep sinh-cutoff1 50)) #:width 500)
(plot (function sinh (flstep sinh-cutoff2 -50) (flstep sinh-cutoff2 50)) #:width 500)
(plot (function sinh (flstep sinh-cutoff3 -50) (flstep sinh-cutoff3 50)) #:width 500)
(plot (function sinh (flstep sinh-cutoff4 -50) (flstep sinh-cutoff4 50)) #:width 500)
(plot (function sinh (flstep sinh-max -100) sinh-max) #:width 500)

(define cosh-cutoff1 (expt 2.0 -26))
(define cosh-cutoff2 (* 0.5 (log 2.0)))
(define cosh-cutoff3 22.0)
(define cosh-cutoff4 (log +max.0))
(define cosh-max 710.475860073943)

(plot (function cosh -5 5))

(plot (function cosh (flstep cosh-cutoff1 -50) (flstep cosh-cutoff1 50)) #:width 500)
(plot (function cosh (flstep cosh-cutoff2 -50) (flstep cosh-cutoff2 50)) #:width 500)
(plot (function cosh (flstep cosh-cutoff3 -50) (flstep cosh-cutoff3 50)) #:width 500)
(plot (function cosh (flstep cosh-cutoff4 -50) (flstep cosh-cutoff4 50)) #:width 500)
(plot (function cosh (flstep cosh-max -100) cosh-max) #:width 500)

(define tanh-cutoff1 (expt 2.0 -55))
(define tanh-cutoff2 1.0)
(define tanh-max 22.0)

(plot (function tanh -5 5))

(plot (function tanh (flstep 0.0 -50) (flstep 0.0 50)))
(plot (function tanh (flstep tanh-cutoff1 -50) (flstep tanh-cutoff1 50)) #:width 500)
(plot (function tanh (flstep tanh-cutoff2 -50) (flstep tanh-cutoff2 50)) #:width 500)

