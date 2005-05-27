
(module texpict-unit mzscheme
  (require (lib "unitsig.ss"))

  (require "private/texpict-sig.ss"
	   "private/common-sig.ss"
	   "private/common-unit.ss"
	   "private/texpict-extra.ss")
	
  (provide texpict@)
  (define texpict@
    (compound-unit/sig
     (import)
     (link
      [common : ((open texpict-common^)
		 (open texpict-internal^))
	      (common@
	       (texpictx : texpict-common-setup^))]
      [texpictx : ((open texpict-extra^)
		   (open texpict-common-setup^))
		(texpict-extra@
		 common)])
     (export (open (common : texpict-common^))
	     (open (texpictx : texpict-extra^))))))

