#lang racket/base

;; xrepl is intended to be loaded from your init file, see the
;; documentation for details.

(require "xrepl.rkt")

;; may want to disable inlining to allow redefinitions
;; (compile-enforce-module-constants #f)

;; start everything
(setup-xrepl-environment)
