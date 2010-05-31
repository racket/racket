#lang scheme/base
(require scheme/class
          "../../syntax.rkt"
         "item.rkt")

(provide slider%)

(defclass slider% item%
  (def/public-unimplemented set-value)
  (def/public-unimplemented get-value)
  (super-new))
