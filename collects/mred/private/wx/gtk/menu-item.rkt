#lang scheme/base
(require scheme/class
          "../../syntax.rkt")

(provide menu-item%)

(defclass menu-item% object%
  (define/public (id) this)
  (super-new))
