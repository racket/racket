
(module file mzscheme
  (require (lib "unit.ss"))

  (require "file-sig.ss")
  (require "file-unit.ss")

  (define-values/invoke-unit/infer dynext:file@)

  (provide-signature-elements dynext:file^))
