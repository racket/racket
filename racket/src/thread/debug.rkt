#lang racket/base

;; Change `debug-select` to enable or disable debugging mode,
;; such as assertions about the current atomicity mode.

(provide debug-select)

(define-syntax-rule (debug-select
                     #:on
                     [on ...]
                     #:off
                     [off ...])
  ;; Select `on` or `off` here:
  (begin off ...))
