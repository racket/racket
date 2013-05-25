(module option mzscheme
  (require mzlib/unit)

  (require "sig.rkt"
           "option-unit.rkt")

  (define-values/invoke-unit/infer compiler:option@)

  (provide-signature-elements compiler:option^))
