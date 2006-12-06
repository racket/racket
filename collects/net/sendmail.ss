(module sendmail mzscheme
  (require (lib "unit.ss") "sendmail-sig.ss" "sendmail-unit.ss")

  (define-values/invoke-unit/infer sendmail@)

  (provide-signature-elements sendmail^))
