#lang racket

(require math/private/log1p
         math/private/expm1
         math/private/sqrt1pm1
         (only-in math/constants +epsilon.0)
         plot
         unstable/flonum)

(define log1p-cutoff1 (* 0.5 +epsilon.0))
(define log1p-cutoff2 1.0)

(plot (function log1p -0.999 10))
(plot (function log1p (- log1p-cutoff2) (flstep (- log1p-cutoff2) 100)))
(plot (function log1p (flstep (- log1p-cutoff1) -50) (flstep (- log1p-cutoff1) 50))
      #:width 500)
(plot (function log1p (flstep log1p-cutoff1 -50) (flstep log1p-cutoff1 50))
      #:width 500)
(plot (function log1p (flstep log1p-cutoff2 -50) (flstep log1p-cutoff2 50))
      #:width 500)

(define expm1-cutoff1 (* 0.5 +epsilon.0))
(define expm1-cutoff2 0.5)

(plot (function expm1 -2 2))
(plot (function expm1 (flstep (- expm1-cutoff2) -50) (flstep (- expm1-cutoff2) 50))
      #:width 500)
(plot (function expm1 (flstep (- expm1-cutoff1) -50) (flstep (- expm1-cutoff1) 50))
      #:width 500)
(plot (function expm1 (flstep expm1-cutoff1 -50) (flstep expm1-cutoff1 50))
      #:width 500)
(plot (function expm1 (flstep expm1-cutoff2 -50) (flstep expm1-cutoff2 50))
      #:width 500)

(define sqrt1pm1-cutoff 0.75)

(plot (function sqrt1pm1 (flstep (- sqrt1pm1-cutoff) -50) (flstep (- sqrt1pm1-cutoff) 50))
      #:width 500)
(plot (function sqrt1pm1 (flstep 0.0 -50) (flstep 0.0 50)))
(plot (function sqrt1pm1 (flstep sqrt1pm1-cutoff -50) (flstep sqrt1pm1-cutoff 50))
      #:width 500)
