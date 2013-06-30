(module option racket/base
  (require racket/unit)

  (require compiler/sig
           compiler/option-unit)

  (define-values/invoke-unit/infer compiler:option@)

  (provide-signature-elements compiler:option^))
