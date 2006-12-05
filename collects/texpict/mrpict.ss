
(module mrpict mzscheme
  (require (lib "unit.ss"))

  (require (lib "mred-sig.ss" "mred")
	   (lib "mred-unit.ss" "mred"))
  (require "private/mrpict-sig.ss"
	   "private/common-sig.ss")
  (require "mrpict-sig.ss"
	   "mrpict-unit.ss")

  (define-compound-unit/infer mrpict+mred@
    (import)
    (export texpict-common^ mrpict-extra^)
    (link standard-mred@ mrpict@))
  
  (define-values/invoke-unit/infer mrpict+mred@)

  (provide-signature-elements texpict-common^ mrpict-extra^))
