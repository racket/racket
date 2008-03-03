#lang setup/infotab

(define pre-install-collection "makefile.ss")
(define virtual-sources '("gl-info.ss"))
(define clean (list (build-path "compiled" "native" (system-library-subpath))
                    "compiled"))
(define compile-omit-paths '("examples"))
