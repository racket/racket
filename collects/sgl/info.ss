(module info setup/infotab
  (define name "sgl")
  (define pre-install-collection "makefile.ss")
  (define compile-omit-files (list))
  (define virtual-sources '("gl-info.ss"))
  (define clean (list (build-path "compiled" "native" (system-library-subpath))
                      "compiled"
                      )))
