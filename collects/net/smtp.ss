(module smtp mzscheme
  (require mzlib/unit "smtp-sig.ss" "smtp-unit.ss")

  (define-values/invoke-unit/infer smtp@)

  (provide-signature-elements smtp^))
