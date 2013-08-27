#lang info

(define collection "frtime")

(define name "FrTime")

(define compile-omit-paths '("demos" "tests"))

(define scribblings '(("scribblings/frtime.scrbl" () (experimental 50))))
(define deps '("srfi-lite-lib"
               "base"
               "compatibility-lib"
               "drracket"
               "gui-lib"
               "pict-lib"
               "string-constants-lib"))
(define build-deps '("draw-doc"
                     "gui-doc"
                     "racket-doc"
                     "scribble-lib"))

(define pkg-desc "The implementation of the FrTime language")

(define pkg-authors '(jay gcooper))
