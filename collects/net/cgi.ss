(module cgi mzscheme
  (require (lib "unit.ss") "cgi-sig.ss" "cgi-unit.ss")

  (define-values/invoke-unit/infer cgi@)

  (provide-signature-elements cgi^))
