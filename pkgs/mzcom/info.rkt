#lang info

(define collection "mzcom")

(define post-install-collection "installer.rkt")

(define scribblings '(("mzcom.scrbl" () (interop))))
(define deps '("base"
               "compatibility-lib"))
(define build-deps '("scheme-lib"
                     "racket-doc"
                     "mysterx"
                     "scribble-lib"))

(define pkg-desc "COM control to instantate a Racket instance")

(define pkg-authors '(mflatt))
