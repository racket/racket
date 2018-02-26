#lang racket/base
(require "linklet.rkt"
         (prefix-in host: '#%linklet)
         "linklet-operation.rkt"
         "../common/reflect-hash.rkt")

;; Run this module before "../host/linklet.rkt" to substitute the
;; implementation in "linklet.rkt"

(define bootstrap-linklet-instance
  (host:primitive-table '#%bootstrap-linklet
                        (linklet-operations=> reflect-hash)))
