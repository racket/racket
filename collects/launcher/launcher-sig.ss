
(module launcher-sig mzscheme
  (require (lib "unitsig.ss"))
  
  (provide launcher^)

  (define-signature launcher^
    (make-mred-launcher
     make-mzscheme-launcher

     make-mred-program-launcher
     make-mzscheme-program-launcher

     mred-program-launcher-path
     mzscheme-program-launcher-path

     install-mred-program-launcher
     install-mzscheme-program-launcher

     mred-launcher-up-to-date?
     mzscheme-launcher-up-to-date?

     mred-launcher-is-directory?
     mzscheme-launcher-is-directory?

     mred-launcher-put-file-extension+style+filters
     mzscheme-launcher-put-file-extension+style+filters

     build-aux-from-path
     current-launcher-variant
     available-mred-variants
     available-mzscheme-variants)))
