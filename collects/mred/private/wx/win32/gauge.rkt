#lang scheme/base
(require scheme/class
          "../../syntax.rkt"
         "item.rkt")

(provide gauge%)

(defclass gauge% item%
  (def/public-unimplemented get-value)
  (def/public-unimplemented set-value)
  (def/public-unimplemented get-range)
  (def/public-unimplemented set-range)
  (super-new))
