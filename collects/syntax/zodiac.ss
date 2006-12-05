
(module zodiac mzscheme
  (require (lib "unit.ss"))

  (require "zodiac-sig.ss")
  (require "zodiac-unit.ss")

  (define-values/invoke-unit/infer zodiac@)

  (provide-signature-elements zodiac^))
