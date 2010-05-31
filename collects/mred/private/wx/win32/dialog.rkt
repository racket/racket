#lang scheme/base
(require scheme/class
          "../../syntax.rkt"
         "window.rkt")

(provide dialog%)

(defclass dialog% window%
  (def/public-unimplemented system-menu)
  (def/public-unimplemented set-title)
  (def/public-unimplemented enforce-size)
  (def/public-unimplemented on-close)
  (def/public-unimplemented on-activate)
  (super-new))
