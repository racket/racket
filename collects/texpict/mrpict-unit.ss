
(module mrpict-unit mzscheme
  (require (lib "unitsig.ss"))

  (require (lib "mred-sig.ss" "mred"))

  (require "private/mrpict-sig.ss"
	   "private/common-sig.ss"
	   "private/common-unit.ss"
	   "private/mrpict-extra.ss")
	
  (provide mrpict@)
  (define mrpict@
    (compound-unit/sig
     (import (MRED : mred^))
     (link [COMMON : ((open texpict-common^)
		      (open texpict-internal^))
		   (common@
		    (MRPICTX : texpict-common-setup^))]
	   [MRPICTX : ((open mrpict-extra^)
		       (open texpict-common-setup^))
		    (mrpict-extra@
		     MRED
		     COMMON)])
     (export (open (COMMON : texpict-common^))
	     (open (MRPICTX : mrpict-extra^))))))

