#lang typed/racket/base

(require "flomap-struct.rkt"
         "flomap-stats.rkt"
         "flomap-pointwise.rkt"
         "flomap-transform.rkt"
         "flomap-gradient.rkt"
         "flomap-effects.rkt"
         "flomap-blur.rkt"
         "flomap-composite.rkt"
         "flomap-resize.rkt")

(require/typed
 "draw-predicates.rkt"
 [opaque Bitmap bitmap?]
 [opaque DC dc?])

(require/typed
 "flomap-convert.rkt"
 [bitmap->flomap  (Bitmap -> flomap)]
 [flomap->bitmap  (flomap -> Bitmap)]
 [draw-flomap     (Integer Integer (DC -> Any) -> flomap)])

(provide (all-from-out "flomap-struct.rkt"
                       "flomap-stats.rkt"
                       "flomap-pointwise.rkt"
                       "flomap-transform.rkt"
                       "flomap-gradient.rkt"
                       "flomap-effects.rkt"
                       "flomap-blur.rkt"
                       "flomap-composite.rkt"
                       "flomap-resize.rkt")
         Bitmap DC
         bitmap->flomap flomap->bitmap draw-flomap)
