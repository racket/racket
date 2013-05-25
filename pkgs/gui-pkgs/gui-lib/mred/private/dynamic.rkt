#lang racket/base

;; This module is for use by racket/gui/dynamic.
;; It is required by mred/mred so that it gets carried
;; along when mred/mred is attached to a new namespace.

(provide kernel-initialized)

(define kernel-initialized 'done)
