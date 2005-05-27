
(module nntp mzscheme
  (require (lib "unitsig.ss"))

  (require "nntp-sig.ss")
  (require "nntp-unit.ss")

  (define-values/invoke-unit/sig net:nntp^
    net:nntp@)

  (provide-signature-elements net:nntp^))
