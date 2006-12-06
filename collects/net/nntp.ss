(module nntp mzscheme
  (require (lib "unit.ss") "nntp-sig.ss" "nntp-unit.ss")

  (define-values/invoke-unit/infer nntp@)

  (provide-signature-elements nntp^))
