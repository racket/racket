
(module sig mzscheme
  (import (lib "unitsig.ss"))
  
  (export setup-option^)

  (define-signature setup-option^
    (verbose
     make-verbose
     compiler-verbose
     clean
     make-zo
     make-so
     make-launchers
     call-install
     pause-on-errors
     specific-collections
     archives)))
