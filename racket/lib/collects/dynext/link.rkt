(module link mzscheme
  (require mzlib/unit)

  (require "link-sig.rkt"
           "link-unit.rkt")

  (define-values/invoke-unit/infer dynext:link@)

  (provide-signature-elements dynext:link^))
