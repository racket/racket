(module texpict-unit mzscheme
  (require mzlib/unit)

  (require "private/texpict-sig.rkt"
           "private/common-sig.rkt"
           "private/common-unit.rkt"
           "private/texpict-extra.rkt")

  (provide texpict@)
  (define-compound-unit/infer texpict@
    (import)
    (export texpict-common^
            texpict-extra^)
    (link common@ texpict-extra@)))
