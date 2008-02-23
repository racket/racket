(module cookie mzscheme
  (require mzlib/unit "cookie-sig.ss" "cookie-unit.ss")

  (provide-signature-elements cookie^)

  (define-values/invoke-unit/infer cookie@))
