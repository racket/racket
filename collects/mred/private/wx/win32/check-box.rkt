#lang scheme/base
(require scheme/class
          "../../syntax.rkt"
         "item.rkt")

(provide check-box%)

(defclass check-box% item%
  (def/public-unimplemented set-value)
  (def/public-unimplemented get-value)
  (super-new))
