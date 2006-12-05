
(module base mzscheme
  (require (lib "unit.ss"))

  (require "../sig.ss")
  (require "sig.ss")

  (require (lib "zodiac-sig.ss" "syntax")
	   (lib "zodiac-unit.ss" "syntax"))

  (require (lib "file-sig.ss" "dynext")
	   (lib "link-sig.ss" "dynext")
	   (lib "compile-sig.ss" "dynext"))

  (require "zlayer.ss"
	   "library.ss"
	   "cstructs.ss"
	   "prephase.ss"
	   "anorm.ss"
	   "const.ss"
	   "known.ss"
	   "analyze.ss"
	   "lift.ss"
	   "closure.ss"
	   "vehicle.ss"
	   "rep.ss"
	   "vmscheme.ss"
	   "vmphase.ss"
	   "vmopt.ss"
	   "vm2c.ss"
	   "toplevel.ss"
	   "driver.ss")

  ;; The core Scheme->C compiler linkage, including everything
  ;;  that's common to MrSpidey and non-MrSpidey compilation.

  (provide base@)

  (define-compound-unit/infer base@
    (import (COMPILE : dynext:compile^)
            (LINK : dynext:link^)
            (DFILE : dynext:file^)
            (OPTIONS : compiler:option^))
    (export compiler:inner^)
    (link
     zodiac@
     zlayer@
     library@
     cstructs@
     prephase@
     anorm@
     const@
     known@
     analyze@
     lift@
     closure@
     vehicle@
     rep@
     vmscheme@
     vmphase@
     vmopt@
     vm2c@
     toplevel@
     driver@)))
