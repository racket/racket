#lang scheme/base
(require scheme/class
          "../../syntax.rkt"
         "window.rkt")

(provide panel%)

(defclass panel% window%
  (def/public-unimplemented get-label-position)
  (def/public-unimplemented set-label-position)
  (def/public-unimplemented on-char)
  (def/public-unimplemented on-event)
  (def/public-unimplemented on-paint)
  (def/public-unimplemented set-item-cursor)
  (def/public-unimplemented get-item-cursor)
  (super-new))
