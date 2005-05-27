
(module link mzscheme
  (require (lib "unitsig.ss"))

  (require "link-sig.ss")
  (require "link-unit.ss")

  (define-values/invoke-unit/sig dynext:link^
    dynext:link@)

  (provide-signature-elements dynext:link^))
