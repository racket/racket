(module cookie mzscheme
  (require (lib "unitsig.ss")
           "cookie-sig.ss"
           "cookie-unit.ss")

  (provide-signature-elements net:cookie^)

  (define-values/invoke-unit/sig net:cookie^
    cookie@))