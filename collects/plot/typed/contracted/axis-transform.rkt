#lang typed/racket/base

(require "../common/types.rkt")

(require/typed/provide
 plot/contracted/axis-transform
 
 [id-function invertible-function]
 [invertible-compose (invertible-function invertible-function -> invertible-function)]
 [invertible-inverse (invertible-function -> invertible-function)]
 
 [id-transform Axis-Transform]
 [apply-axis-transform (Axis-Transform Real Real -> invertible-function)]
 [make-axis-transform (invertible-function -> Axis-Transform)]
 [axis-transform-compose (Axis-Transform Axis-Transform -> Axis-Transform)]
 [axis-transform-append (Axis-Transform Axis-Transform Real -> Axis-Transform)]
 [axis-transform-bound (Axis-Transform Real Real -> Axis-Transform)]

 [log-transform Axis-Transform]
 [cbrt-transform Axis-Transform]
 [hand-drawn-transform (Real -> Axis-Transform)]
 [stretch-transform (Real Real Real -> Axis-Transform)]
 [collapse-transform (Real Real -> Axis-Transform)]
 )
