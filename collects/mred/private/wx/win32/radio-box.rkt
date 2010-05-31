#lang scheme/base
(require scheme/class
          "../../syntax.rkt"
         "item.rkt")

(provide radio-box%)

(defclass radio-box% item%
  (def/public-unimplemented button-focus)
  (def/public-unimplemented set-selection)
  (def/public-unimplemented number)
  (def/public-unimplemented get-selection)
  (super-new))
