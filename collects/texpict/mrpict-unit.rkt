
(module mrpict-unit mzscheme
  (require mzlib/unit)

  (require racket/draw/draw-sig)

  (require "private/mrpict-sig.ss"
	   "private/common-sig.ss"
	   "private/common-unit.ss"
	   "private/mrpict-extra.ss")
	
  (provide mrpict@)
  (define-compound-unit/infer mrpict@
    (import draw^)
    (export texpict-common^ mrpict-extra^)
    (link common@ mrpict-extra@)))
