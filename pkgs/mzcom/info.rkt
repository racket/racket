#lang info

(define collection "mzcom")

(define post-install-collection "installer.rkt")

(define scribblings '(("mzcom.scrbl" () (interop))))
(define deps '("base"
               "compatibility-lib"))
(define build-deps '("racket-doc"
                     "mysterx"
                     "scribble-lib"))
