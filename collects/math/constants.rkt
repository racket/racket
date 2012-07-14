#lang typed/racket/base

#|
TODO

Rational approximations to all of these
|#

(provide
 ;pi pi.f
 euler.0 euler.f
 golden-ratio.0 golden-ratio.f)

;(define pi 3.1415926535897932)
;(define pi.f 3.141592653f0)

(define euler.0 0.57721566490153286)
(define euler.f 0.57721566f0)

(define golden-ratio.0 1.6180339887498948)
(define golden-ratio.f 1.6180339f0)
