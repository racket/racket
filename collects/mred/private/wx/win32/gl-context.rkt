#lang scheme/base
(require scheme/class
          "../../syntax.rkt")

(provide gl-context%)

(defclass gl-context% object%
  (def/public-unimplemented call-as-current)
  (def/public-unimplemented swap-buffers)
  (def/public-unimplemented ok?)
  (super-new))
