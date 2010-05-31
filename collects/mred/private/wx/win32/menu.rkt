#lang scheme/base
(require scheme/class
          "../../syntax.rkt")

(provide menu%)

(defclass menu% object%
  (def/public-unimplemented select)
  (def/public-unimplemented get-font)
  (def/public-unimplemented set-width)
  (def/public-unimplemented set-title)
  (def/public-unimplemented set-label)
  (def/public-unimplemented set-help-string)
  (def/public-unimplemented number)
  (def/public-unimplemented enable)
  (def/public-unimplemented check)
  (def/public-unimplemented checked?)
  (def/public-unimplemented append-separator)
  (def/public-unimplemented delete-by-position)
  (def/public-unimplemented delete)
  (def/public-unimplemented append)
  (super-new))
