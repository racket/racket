#lang info

(define collection 'multi)

(define build-deps '("scheme-lib"
                     "draw-doc"
                     "gui-doc"
                     "pict-doc"
                     "scribble-doc"
                     "web-server-doc"
                     "base"
                     "gui-lib"
                     "pict-lib"
                     "scribble-lib"
                     "slideshow-lib"
                     "racket-doc"))
(define update-implies '("slideshow-lib"))

(define pkg-desc "documentation part of \"slideshow\"")

(define pkg-authors '(mflatt robby))
