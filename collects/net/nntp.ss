(module nntp mzscheme
  (require mzlib/unit "nntp-sig.ss" "nntp-unit.ss")

  (define-values/invoke-unit/infer nntp@)

  (provide-signature-elements nntp^))
