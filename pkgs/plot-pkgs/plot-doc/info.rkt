#lang info

(define collection 'multi)

(define deps '("base"
               "plot-lib"
               "plot-gui-lib"
               "unstable-latent-contract-lib"))

(define build-deps '("db-doc"
                     "db-lib"
                     "draw-doc"
                     "draw-lib"
                     "gui-doc"
                     "gui-lib"
                     "pict-doc"
                     "pict-lib"
                     "plot-compat"
                     "racket-doc"
                     "scribble-lib"
                     "slideshow-doc"
                     "slideshow-lib"
                     "srfi-doc"
                     "unstable-contract-lib"
                     "unstable-doc"))

(define pkg-desc "Documentation for plot")

(define pkg-authors '(ntoronto))
