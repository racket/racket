
(module launcher-sig mzscheme
  (require (lib "unitsig.ss"))
  
  (provide launcher-maker^)

  (define-signature launcher-maker^
    (make-mred-launcher
     make-mzscheme-launcher

     make-mred-program-launcher
     make-mzscheme-program-launcher

     mred-program-launcher-path
     mzscheme-program-launcher-path

     install-mred-program-launcher
     install-mzscheme-program-launcher)))

