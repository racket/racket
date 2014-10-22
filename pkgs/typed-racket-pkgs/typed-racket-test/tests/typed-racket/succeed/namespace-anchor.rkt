#lang typed/racket/base

;; Tests namespace anchors. Not a unit test because `define-namespace-anchor`
;; only works at the top-level or in a module.

(: x Namespace-Anchor)
(define-namespace-anchor x)
