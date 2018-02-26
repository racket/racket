#lang racket/base
(require "linklet-info.rkt"
         "../host/linklet.rkt"
         (prefix-in bootstrap: "../run/linklet.rkt"))

(provide skip-abi-imports
         linklets-are-source-mode?)

;; Skip over syntax literals and instance:
(define (skip-abi-imports l)
  (list-tail l 2))

;; Detect source mode, which enables final assembly
(define (linklets-are-source-mode? linklets)
  (define bootstrap-mode?
    (eq? bootstrap:compile-linklet compile-linklet))
  (and bootstrap-mode?
       (not (zero? (hash-count linklets)))
       (bootstrap:linklet-as-s-expr?
        (linklet-info-linklet
         (hash-iterate-value linklets (hash-iterate-first linklets))))))
