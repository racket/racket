#lang scheme/base
(require scheme/class
          "../../syntax.rkt")

(provide gl-config%)

(defclass gl-config% object%
  (def/public-unimplemented get-double-buffered)
  (def/public-unimplemented set-double-buffered)
  (def/public-unimplemented get-stereo)
  (def/public-unimplemented set-stereo)
  (def/public-unimplemented get-stencil-size)
  (def/public-unimplemented set-stencil-size)
  (def/public-unimplemented get-accum-size)
  (def/public-unimplemented set-accum-size)
  (def/public-unimplemented get-depth-size)
  (def/public-unimplemented set-depth-size)
  (def/public-unimplemented get-multisample-size)
  (def/public-unimplemented set-multisample-size)
  (super-new))
