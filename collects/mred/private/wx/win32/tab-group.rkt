#lang scheme/base
(require scheme/class
          "../../syntax.rkt"
         "item.rkt")

(provide tab-group%)

(defclass tab-group% item%
  (def/public-unimplemented button-focus)
  (def/public-unimplemented set)
  (def/public-unimplemented delete)
  (def/public-unimplemented append)
  (def/public-unimplemented set-selection)
  (def/public-unimplemented number)
  (def/public-unimplemented get-selection)
  (super-new))
