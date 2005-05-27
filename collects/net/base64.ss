

(module base64 mzscheme
  (require (lib "unitsig.ss"))

  (require "base64-sig.ss")
  (require "base64-unit.ss")

  (define-values/invoke-unit/sig net:base64^
    net:base64@)

  (provide-signature-elements net:base64^))
