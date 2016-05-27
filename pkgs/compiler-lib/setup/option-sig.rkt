(module option-sig racket/base
  (require racket/unit)
  
  (provide setup-option^)

  (define-signature setup-option^
    (setup-program-name
     verbose
     make-verbose
     compiler-verbose
     clean
     compile-mode
     make-only
     make-zo
     make-info-domain
     make-foreign-libs
     make-launchers
     make-docs
     make-user
     make-planet
     avoid-main-installation
     make-tidy
     make-doc-index
     check-dependencies
     fix-dependencies
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
