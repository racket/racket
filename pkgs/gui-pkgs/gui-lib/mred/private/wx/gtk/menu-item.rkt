#lang racket/base
(require racket/class
          "../../syntax.rkt")

(provide 
 (protect-out menu-item%))

(defclass menu-item% object%
  (define/public (id) this)
  (super-new))
