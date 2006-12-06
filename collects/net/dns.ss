(module dns mzscheme
  (require (lib "unit.ss") "dns-sig.ss" "dns-unit.ss")

  (define-values/invoke-unit/infer dns@)

  (provide-signature-elements dns^))
