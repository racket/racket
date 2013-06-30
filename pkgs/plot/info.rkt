#lang setup/infotab

(define collection "plot")

(define scribblings '(["scribblings/plot.scrbl" (multi-page) (gui-library)]))

(define compile-omit-paths '("tests"))
(define deps '("typed-racket-more"
               "base"
               "compatibility-lib"
               "draw-lib"
               "gui-lib"
               "pict-lib"
               "snip-lib"
               "typed-racket-lib"
               "unstable"
               "unstable-contract-lib"
               "unstable-latent-contract-lib"
               "unstable-parameter-group-lib"))
(define build-deps '("scribble-lib"
                     "slideshow-lib"))
