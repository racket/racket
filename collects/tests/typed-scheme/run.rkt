#lang scheme/base

(require "main.ss")
(current-namespace (make-base-namespace))
(unless (= 0 (go/text (member "unit" (vector->list (current-command-line-arguments)))))
  (error "Typed Scheme Tests did not pass."))
