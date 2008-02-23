
(module option-sig mzscheme
  (require mzlib/unit)
  
  (provide setup-option^)

  (define-signature setup-option^
    (verbose
     make-verbose
     compiler-verbose
     clean
     compile-mode
     make-zo
     make-info-domain
     make-launchers
     make-docs
     make-user
     make-planet
     call-install
     call-post-install
     pause-on-errors
     force-unpacks
     doc-pdf-dest
     specific-collections
     specific-planet-dirs
     archives
     current-target-directory-getter
     current-target-plt-directory-getter)))
