#lang scheme/base
(require scheme/class
          "../../syntax.rkt")

(provide clipboard-driver%
         has-x-selection?)

(define (has-x-selection?) #f)

(defclass clipboard-driver% object%
  (init x-selection?) ; always #f
  (super-new))
