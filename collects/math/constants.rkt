#lang typed/racket/base

#|
TODO

-max.f -min.f +min.f +max.f
|#

(require/typed
 unstable/flonum
 [-max.0  Negative-Float]
 [-min.0  Negative-Float]
 [+min.0  Positive-Float]
 [+max.0  Positive-Float])

(provide ;pi pi.f
         euler.0 euler.f
         golden-ratio.0 golden-ratio.f
         +epsilon.0
         -max.0 -min.0 +min.0 +max.0
         )

;(define pi 3.1415926535897932)
;(define pi.f 3.141592653f0)

(define euler.0 0.57721566490153286)
(define euler.f 0.57721566f0)

(define golden-ratio.0 1.6180339887498948)
(define golden-ratio.f 1.6180339f0)

;; The smallest flonum that can be added to 1.0 to get a result != 1.0
(define +epsilon.0 (expt 2.0 -52))
