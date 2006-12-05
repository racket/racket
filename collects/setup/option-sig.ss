
(module option-sig mzscheme
  (require (lib "unit.ss"))
  
  (provide setup-option^)

  (define-signature setup-option^
    (verbose
     make-verbose
     compiler-verbose
     clean
     compile-mode
     make-zo
     make-so
     make-info-domain
     make-launchers
     call-install
     pause-on-errors
     force-unpacks
     specific-collections
     specific-planet-dirs
     archives
     current-target-directory-getter
     current-target-plt-directory-getter)))
