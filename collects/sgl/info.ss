(module info (lib "infotab.ss" "setup")
  (define name "sgl")
  (define doc.txt "doc.txt")
  (define pre-install-collection "makefile.ss")
  (define compile-omit-files (list))
  (define clean (list (build-path "compiled" "native" (system-library-subpath))
                      "compiled"
                      )))
