#lang racket/base

(require racket/contract
         "../common/contract.rkt"
         "../common/sample.rkt"
         "../common/parameters.rkt")

(provide (all-defined-out))

(define function->sampler (make-function->sampler plot-x-transform))
(define inverse->sampler (make-function->sampler plot-y-transform))
(define 2d-function->sampler (make-2d-function->sampler plot-x-transform plot-y-transform))
