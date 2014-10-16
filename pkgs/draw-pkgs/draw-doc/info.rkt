#lang info

(define collection 'multi)

(define build-deps '("gui-doc"
                     "pict-doc"
                     "at-exp-lib"
                     "base"
                     "gui-lib"
                     "pict-lib"
                     "scribble-lib"
                     "draw-lib"
                     "racket-doc"))
(define update-implies '("draw-lib"))

(define pkg-desc "documentation part of \"draw\"")

(define pkg-authors '(mflatt))
