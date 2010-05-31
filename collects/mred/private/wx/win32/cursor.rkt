#lang scheme/base
(require scheme/class
          "../../syntax.rkt")

(provide cursor-driver%)

(defclass cursor-driver% object%
  (def/public-unimplemented ok?)
  (super-new))
