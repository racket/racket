#lang scheme/base
(require scheme/class
          "../../syntax.rkt")

(provide cursor-driver%)

(defclass cursor-driver% object%
  (def/public-unimplemented ok?)
  (define/public (set-standard sym) (void))
  (super-new))
