#lang info

(define collection "plot")

(define scribblings '(["scribblings/plot.scrbl" (multi-page) (gui-library)]))

(define compile-omit-paths '("tests"))
(define deps '("srfi-lite-lib"
               "typed-racket-more"
               "base"
               "compatibility-lib"
               "db-lib"
               "draw-lib"
               "gui-lib"
               "pict-lib"
               "snip-lib"
               "typed-racket-lib"
               "unstable-lib"
               "unstable-flonum-lib"
               "unstable-contract-lib"
               "unstable-latent-contract-lib"
               "unstable-parameter-group-lib"))
(define build-deps '("db-doc"
                     "draw-doc"
                     "gui-doc"
                     "pict-doc"
                     "racket-doc"
                     "slideshow-doc"
                     "srfi-doc"
                     "scribble-lib"
                     "slideshow-lib"
                     "unstable-doc"))

(define pkg-desc "Libraries for graph plotting")

(define pkg-authors '(ntoronto))
