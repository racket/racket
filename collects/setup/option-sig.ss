
(module option-sig mzscheme
  (require (lib "unitsig.ss"))
  
  (provide setup-option^)

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
     force-unpacks
     specific-collections
     archives
     current-target-directory-getter
     current-target-plt-directory-getter)))
