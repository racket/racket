
(module zodiac mzscheme
  (require (lib "unitsig.ss"))

  (require "zodiac-sig.ss")
  (require "zodiac-unit.ss")

  (define-values/invoke-unit/sig zodiac^
    zodiac@)

  (provide-signature-elements zodiac^))
