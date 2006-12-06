(module smtp mzscheme
  (require (lib "unit.ss") "smtp-sig.ss" "smtp-unit.ss")

  (define-values/invoke-unit/infer smtp@)

  (provide-signature-elements smtp^))
