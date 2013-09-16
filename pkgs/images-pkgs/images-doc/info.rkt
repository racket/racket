#lang info

(define collection 'multi)

(define deps '("base"
               "images-lib"))
(define build-deps '("draw-doc"
                     "gui-doc"
                     "pict-doc"
                     "slideshow-doc"
                     "typed-racket-doc"
                     "draw-lib"
                     "gui-lib"
                     "pict-lib"
                     "racket-doc"
                     "scribble-lib"
                     "slideshow-lib"
                     "typed-racket-lib"
                     "unstable-latent-contract-lib"))

(define pkg-desc "Documentation for images-lib")

(define pkg-authors '(ntoronto))
