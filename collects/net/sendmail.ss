
(module sendmail mzscheme
  (require (lib "unitsig.ss"))

  (require "sendmail-sig.ss")
  (require "sendmail-unit.ss")

  (define-values/invoke-unit/sig net:sendmail^
    net:sendmail@)

  (provide-signature-elements net:sendmail^))
