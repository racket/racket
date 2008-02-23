
(module link mzscheme
  (require mzlib/unit)

  (require "link-sig.ss")
  (require "link-unit.ss")

  (define-values/invoke-unit/infer dynext:link@)

  (provide-signature-elements dynext:link^))
