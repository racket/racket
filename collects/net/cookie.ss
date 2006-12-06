(module cookie mzscheme
  (require (lib "unit.ss") "cookie-sig.ss" "cookie-unit.ss")

  (provide-signature-elements cookie^)

  (define-values/invoke-unit/infer cookie@))
