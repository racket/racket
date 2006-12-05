
(module texpict-unit mzscheme
  (require (lib "unit.ss"))

  (require "private/texpict-sig.ss"
	   "private/common-sig.ss"
	   "private/common-unit.ss"
	   "private/texpict-extra.ss")
	
  (provide texpict@)
  (define-compound-unit/infer texpict@
    (import)
    (export texpict-common^
            texpict-extra^)
    (link common@ texpict-extra@)))

