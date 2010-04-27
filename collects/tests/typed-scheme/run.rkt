#lang scheme/base

(require "main.ss")
(current-namespace (make-base-namespace))
(unless (= 0 (go/text))
  (error "Typed Scheme Tests did not pass."))
