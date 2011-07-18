#lang racket/base

;; This file is intended to be loaded from your init file (evaluatue
;; (find-system-path 'init-file) to see where that is on your OS.)

(require "xrepl.rkt")

;; may want to disable inlining to allow redefinitions
;; (compile-enforce-module-constants #f)

;; create the command repl reader, and value-saving evaluator
(current-prompt-read (make-xrepl-reader))
(current-eval (make-xrepl-evaluator (current-eval)))
