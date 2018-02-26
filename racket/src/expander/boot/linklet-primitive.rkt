#lang racket/base
(require "../host/linklet.rkt"
         "../common/reflect-hash.rkt"
         "../run/linklet-operation.rkt")

(provide linklet-primitives)

(define linklet-primitives
  (linklet-operations=> reflect-hash))
