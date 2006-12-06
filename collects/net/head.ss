(module head mzscheme
  (require (lib "unit.ss") "head-sig.ss" "head-unit.ss")

  (define-values/invoke-unit/infer head@)

  (provide-signature-elements head^))
