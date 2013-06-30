#lang setup/infotab

(define collection "make")

(define scribblings '(("make.scrbl" (multi-page) (tool-library))))
(define deps '("base"
               "compiler-lib"
               "compatibility-lib"))
(define build-deps '("scribble-lib"))
