#lang scheme/base
(require scheme/class
          "../../syntax.rkt")

(provide menu-item%)

(defclass menu-item% object%
  (def/public-unimplemented id)
  (super-new))
