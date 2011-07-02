(module base mzscheme
  (require mzlib/unit)

  (require "../sig.rkt"
           "sig.rkt")

  (require syntax/zodiac-sig
           syntax/zodiac-unit)

  (require dynext/file-sig
           dynext/link-sig
           dynext/compile-sig)

  (require "zlayer.rkt"
           "library.rkt"
           "cstructs.rkt"
           "prephase.rkt"
           "anorm.rkt"
           "const.rkt"
           "known.rkt"
           "analyze.rkt"
           "lift.rkt"
           "closure.rkt"
           "vehicle.rkt"
           "rep.rkt"
           "vmscheme.rkt"
           "vmphase.rkt"
           "vmopt.rkt"
           "vm2c.rkt"
           "toplevel.rkt"
           "driver.rkt")

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
