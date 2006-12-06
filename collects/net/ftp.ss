(module ftp mzscheme
  (require (lib "unit.ss") "ftp-sig.ss" "ftp-unit.ss")

  (define-values/invoke-unit/infer ftp@)

  (provide-signature-elements ftp^))
