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

;; This is a copy of the definition in typed/mred/mred
;; but requiring that produces an error when building
;; the docs
(define-type Bitmap%
  (Class [get-width (-> Integer)]
         [get-height (-> Integer)]
         [get-argb-pixels
          (case-> 
           (Integer Integer Integer Integer Bytes [#:unscaled? Boolean]
                    -> Void)
           (Integer Integer Integer Integer Bytes Boolean [#:unscaled? Boolean]
                    -> Void)
           (Integer Integer Integer Integer Bytes Boolean Boolean [#:unscaled? Boolean]
                    -> Void))]))

(require/typed
 "flomap-convert.rkt"
 [bitmap->flomap  ((Instance Bitmap%) [#:unscaled? Any] -> flomap)]
 [flomap->bitmap  (flomap [#:backing-scale Positive-Real] -> (Instance Bitmap%))]
 [draw-flomap     ((Any -> Any) Integer Integer -> flomap)])

(provide (all-from-out "flomap-struct.rkt"
                       "flomap-stats.rkt"
                       "flomap-pointwise.rkt"
                       "flomap-transform.rkt"
                       "flomap-gradient.rkt"
                       "flomap-effects.rkt"
                       "flomap-blur.rkt"
                       "flomap-composite.rkt"
                       "flomap-resize.rkt")
         bitmap->flomap flomap->bitmap draw-flomap)
