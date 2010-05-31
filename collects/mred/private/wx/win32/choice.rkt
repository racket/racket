#lang scheme/base
(require scheme/class
          "../../syntax.rkt"
         "item.rkt")

(provide choice%)

(defclass choice% item%
  (def/public-unimplemented set-selection)
  (def/public-unimplemented get-selection)
  (def/public-unimplemented number)
  (def/public-unimplemented clear)
  (def/public-unimplemented append)
  (super-new))
