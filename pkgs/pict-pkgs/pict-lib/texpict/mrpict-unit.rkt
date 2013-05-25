(module mrpict-unit mzscheme
  (require mzlib/unit)

  (require racket/draw/draw-sig)

  (require "private/mrpict-sig.rkt"
           "private/common-sig.rkt"
           "private/common-unit.rkt"
           "private/mrpict-extra.rkt")

  (provide mrpict@)
  (define-compound-unit/infer mrpict@
    (import draw^)
    (export texpict-common^ mrpict-extra^)
    (link common@ mrpict-extra@)))
