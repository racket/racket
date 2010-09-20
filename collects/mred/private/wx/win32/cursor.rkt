#lang scheme/base
(require scheme/class
          "../../syntax.rkt")

(provide cursor-driver%)

(defclass cursor-driver% object%
  (define/public (set-standard c) (void))

  (def/public-unimplemented ok?)
  (super-new))
