(module sendmail mzscheme
  (require mzlib/unit "sendmail-sig.ss" "sendmail-unit.ss")

  (define-values/invoke-unit/infer sendmail@)

  (provide-signature-elements sendmail^))
