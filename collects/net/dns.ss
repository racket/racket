(module dns mzscheme
  (require mzlib/unit "dns-sig.ss" "dns-unit.ss")

  (define-values/invoke-unit/infer dns@)

  (provide-signature-elements dns^))
