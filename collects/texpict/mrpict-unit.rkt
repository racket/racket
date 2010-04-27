
(module mrpict-unit mzscheme
  (require mzlib/unit)

  (require mred/mred-sig)

  (require "private/mrpict-sig.ss"
	   "private/common-sig.ss"
	   "private/common-unit.ss"
	   "private/mrpict-extra.ss")
	
  (provide mrpict@)
  (define-compound-unit/infer mrpict@
    (import mred^)
    (export texpict-common^ mrpict-extra^)
    (link common@ mrpict-extra@)))
