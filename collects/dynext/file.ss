
(module file mzscheme
  (require (lib "unitsig.ss"))

  (require "file-sig.ss")
  (require "file-unit.ss")

  (define-values/invoke-unit/sig dynext:file^
    dynext:file@)

  (provide-signature-elements dynext:file^))
