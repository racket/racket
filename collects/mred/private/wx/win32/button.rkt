#lang scheme/base
(require scheme/class
          "../../syntax.rkt"
         "item.rkt")

(provide button%)

(defclass button% item%
  (def/public-unimplemented set-border)
  (super-new))
