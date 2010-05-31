#lang scheme/base
(require scheme/class
          "../../syntax.rkt"
         "item.rkt")

(provide message%)

(defclass message% item%
  (def/public-unimplemented get-font)
  (super-new))
