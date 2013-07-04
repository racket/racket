#lang info

(define collection "images")

(define deps '("base"
               "draw-lib"
               "gui-lib"
               "string-constants-lib"
               "unstable-latent-contract-lib"
               "unstable-parameter-group-lib"
               "typed-racket-lib"))
(define build-deps '("pict-lib"
                     "scribble-lib"
                     "slideshow-lib"
                     "racket-doc"
                     "unstable-latent-contract-lib"
                     "unstable-parameter-group-lib"))

(define scribblings '(["scribblings/images.scrbl" (multi-page) (gui-library)]))

(define compile-omit-paths '("tests"))
