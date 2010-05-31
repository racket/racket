#lang scheme/base
(require scheme/class
          "../../syntax.rkt"
         "item.rkt")

(provide list-box%)

(defclass list-box% item%
  (def/public-unimplemented get-label-font)
  (def/public-unimplemented set-string)
  (def/public-unimplemented set-first-visible-item)
  (def/public-unimplemented set)
  (def/public-unimplemented get-selections)
  (def/public-unimplemented get-first-item)
  (def/public-unimplemented number-of-visible-items)
  (def/public-unimplemented number)
  (def/public-unimplemented get-selection)
  (def/public-unimplemented set-data)
  (def/public-unimplemented get-data)
  (def/public-unimplemented selected?)
  (def/public-unimplemented set-selection)
  (def/public-unimplemented select)
  (def/public-unimplemented delete)
  (def/public-unimplemented clear)
  (def/public-unimplemented append)
  (super-new))
