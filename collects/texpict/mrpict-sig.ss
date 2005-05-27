
(module mrpict-sig mzscheme
  (require (lib "unitsig.ss"))
  
  (require "private/common-sig.ss")
  (require "private/mrpict-sig.ss")
  
  (provide mrpict^)
  (define-signature mrpict^
    ((open texpict-common^)
     (open mrpict-extra^))))

