#lang racket

(require math/private/gamma
         plot
         unstable/flonum)

(time (plot (function gamma -5 5)))
(time (plot (function gamma 9 11)))
(time (plot (function gamma -11 -9)))

(define gamma-cutoff0 1e-10)
(define gamma-cutoff1 140.0)

(define +gamma-min 5.56268464626801e-309)
(define +gamma-max 171.6243769563027)

(plot (function gamma (flstep +gamma-min -20) (flstep +gamma-min 20)))
(plot (function gamma (flstep +gamma-max -20) (flstep +gamma-max 20)))
(plot (function gamma (flstep gamma-cutoff0 -20) (flstep gamma-cutoff0 20)))
(plot (function gamma (flstep gamma-cutoff1 -20) (flstep gamma-cutoff1 20)))
