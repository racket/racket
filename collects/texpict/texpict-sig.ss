
(module texpict-sig mzscheme
  (require (lib "unitsig.ss"))
  
  (require "private/common-sig.ss")
  (require "private/texpict-sig.ss")
  
  (provide texpict^)
  (define-signature texpict^
    ((open texpict-common^)
     (open texpict-extra^))))

