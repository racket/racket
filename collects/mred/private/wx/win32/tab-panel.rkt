#lang scheme/base
(require scheme/class
          "../../syntax.rkt"
         "window.rkt")

(provide tab-panel%)

(defclass tab-panel% window%
  (super-new))
