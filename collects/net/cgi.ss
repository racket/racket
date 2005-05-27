
(module cgi mzscheme
  (require (lib "unitsig.ss"))

  (require "cgi-sig.ss")
  (require "cgi-unit.ss")

  (define-values/invoke-unit/sig net:cgi^
    net:cgi@)

  (provide-signature-elements net:cgi^))
