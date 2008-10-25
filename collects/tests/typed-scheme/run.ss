#lang scheme/base

(require "main.ss")
(current-namespace (make-base-namespace))
(unless (= 0 (parameterize ([error-display-handler void])
               (go/text)))
  (error "Typed Scheme Tests did not pass."))
