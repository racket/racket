#lang racket

(require math/private/gamma
         math/private/log-gamma
         plot
         unstable/flonum)

(time (plot (function (λ (x) (log (abs (gamma x)))) -5 5)))
(time (plot (function log-gamma -5 5)))

(define log-gamma-cutoff1 143.0)
(define log-gamma-cutoff2 1.0e7)

(plot (function log-gamma 0.0 (flstep 0.0 100)))
(plot (function log-gamma (flstep 0.5 -50) (flstep 0.5 50)))
(plot (function log-gamma (flstep 1.0 -50) (flstep 1.0 50)))
(plot (function log-gamma (flstep 2.0 -50) (flstep 2.0 50)))
(plot (function log-gamma (flstep 3.0 -50) (flstep 3.0 50)))
(plot (function log-gamma (flstep 4.0 -50) (flstep 4.0 50)))
(plot (function log-gamma (flstep 5.0 -50) (flstep 5.0 50)))
(plot (function log-gamma (flstep log-gamma-cutoff1 -50) (flstep log-gamma-cutoff1 50)))
(plot (function log-gamma (flstep log-gamma-cutoff2 -50) (flstep log-gamma-cutoff2 50)))
