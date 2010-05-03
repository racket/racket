#lang racket/base
(require racket/vector)

(require "main.ss")
(current-namespace (make-base-namespace))
(unless (= 0 (go/text (vector-member "unit" (current-command-line-arguments))))
  (error "Typed Scheme Tests did not pass."))
