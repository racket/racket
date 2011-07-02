(module zodiac mzscheme
  (require mzlib/unit)

  (require "zodiac-sig.rkt"
           "zodiac-unit.rkt")

  (define-values/invoke-unit/infer zodiac@)

  (provide-signature-elements zodiac^))
