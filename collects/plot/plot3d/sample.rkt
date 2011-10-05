#lang racket/base

(require racket/contract
         "../common/contract.rkt"
         "../common/sample.rkt"
         "../common/parameters.rkt")

(provide (all-defined-out))

(define 2d-function->sampler (make-2d-function->sampler plot-x-transform plot-y-transform))
(define 3d-function->sampler
  (make-3d-function->sampler plot-x-transform plot-y-transform plot-z-transform))
