#lang scheme/base
(require scheme/class
          "../../syntax.rkt"
         "window.rkt")

(provide item%)

(defclass item% window%
  (def/public-unimplemented set-label)
  (def/public-unimplemented get-label)
  (def/public-unimplemented command)
  (super-new))
