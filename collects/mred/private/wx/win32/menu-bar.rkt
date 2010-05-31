#lang scheme/base
(require scheme/class
          "../../syntax.rkt")

(provide menu-bar%)

(defclass menu-bar% object%
  (def/public-unimplemented set-label-top)
  (def/public-unimplemented number)
  (def/public-unimplemented enable-top)
  (def/public-unimplemented delete)
  (def/public-unimplemented append)
  (super-new))
