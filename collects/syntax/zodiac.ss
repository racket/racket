
(module zodiac mzscheme
  (require mzlib/unit)

  (require "zodiac-sig.ss")
  (require "zodiac-unit.ss")

  (define-values/invoke-unit/infer zodiac@)

  (provide-signature-elements zodiac^))
