
(module smtp mzscheme
  (require (lib "unitsig.ss"))

  (require "smtp-sig.ss")
  (require "smtp-unit.ss")

  (define-values/invoke-unit/sig net:smtp^
    net:smtp@)

  (provide-signature-elements net:smtp^))
