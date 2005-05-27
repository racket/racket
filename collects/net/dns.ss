
(module dns mzscheme
  (require (lib "unitsig.ss"))

  (require "dns-sig.ss")
  (require "dns-unit.ss")

  (define-values/invoke-unit/sig net:dns^
    net:dns@)

  (provide-signature-elements net:dns^))
