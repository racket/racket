(module option racket/base
  (require racket/unit)

  (require "sig.rkt"
           "option-unit.rkt")

  (define-values/invoke-unit/infer compiler:option@)

  (provide-signature-elements compiler:option^))
