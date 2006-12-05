
(module mrpict-unit mzscheme
  (require (lib "unit.ss"))

  (require (lib "mred-sig.ss" "mred"))

  (require "private/mrpict-sig.ss"
	   "private/common-sig.ss"
	   "private/common-unit.ss"
	   "private/mrpict-extra.ss")
	
  (provide mrpict@)
  (define-compound-unit/infer mrpict@
    (import mred^)
    (export texpict-common^ mrpict-extra^)
    (link common@ mrpict-extra@)))