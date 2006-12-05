
(module link mzscheme
  (require (lib "unit.ss"))

  (require "link-sig.ss")
  (require "link-unit.ss")

  (define-values/invoke-unit/infer dynext:link@)

  (provide-signature-elements dynext:link^))
