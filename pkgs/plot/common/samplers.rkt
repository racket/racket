#lang racket/base

;; Functions that sample from functions, and functions that create memoized samplers.

(require racket/match racket/flonum racket/math racket/contract racket/list
         unstable/latent-contract/defthing
         "parameters.rkt"
         "sample.rkt"
         "math.rkt")

(provide (all-defined-out))

(defthing function->sampler ((real? . -> . real?) ivl? . -> . sampler/c)
  (make-function->sampler plot-x-transform))

(defthing inverse->sampler ((real? . -> . real?) ivl? . -> . sampler/c)
  (make-function->sampler plot-y-transform))

(defthing 2d-function->sampler (-> (real? real? . -> . real?)
                                   (vector/c ivl? ivl?)
                                   2d-sampler/c)
  (make-2d-function->sampler plot-x-transform plot-y-transform))

(defthing 3d-function->sampler (-> (real? real? real? . -> . real?)
                                   (vector/c ivl? ivl? ivl?)
                                   3d-sampler/c)
  (make-3d-function->sampler plot-x-transform plot-y-transform plot-z-transform))
