
(module head mzscheme
  (require (lib "unitsig.ss"))

  (require "head-sig.ss")
  (require "head-unit.ss")

  (define-values/invoke-unit/sig net:head^
    net:head@)

  (provide-signature-elements net:head^))
