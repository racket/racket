#lang racket/base
(require "../host/linklet.rkt"
         "../compile/linklet.rkt"
         "../common/reflect-hash.rkt"
         "../run/linklet-operation.rkt")

(provide linklet-primitives
         linklet-expander-primitives)

(define linklet-primitives
  (linklet-operations=> reflect-hash))

(define linklet-expander-primitives
  (reflect-hash linklet-directory?
                linklet-directory->hash
                hash->linklet-directory
                linklet-bundle?
                linklet-bundle->hash
                hash->linklet-bundle))
