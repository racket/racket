
(module option-sig mzscheme
  (require mzlib/unit)
  
  (provide setup-option^)

  (define-signature setup-option^
    (setup-program-name
     verbose
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
     avoid-main-installation
     call-install
     call-post-install
     pause-on-errors
     parallel-workers
     force-unpacks
     doc-pdf-dest
     specific-collections
     specific-planet-dirs
     archives
     archive-implies-reindex
     current-target-directory-getter
     current-target-plt-directory-getter)))
