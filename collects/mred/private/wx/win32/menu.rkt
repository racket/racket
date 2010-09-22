#lang scheme/base
(require scheme/class
          "../../syntax.rkt")

(provide menu%)

(defclass menu% object%
  (init label
        callback
        font)

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
  (def/public-unimplemented delete-by-position)
  (def/public-unimplemented delete)

  (public [append-item append])
  (define (append-item i label help-str-or-submenu chckable?)
    (void))

  (define/public (append-separator) 
    (void))

  (super-new))
