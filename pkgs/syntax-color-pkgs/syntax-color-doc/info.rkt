#lang info

(define collection 'multi)

(define build-deps '("gui-doc"
                     "scribble-doc"
                     "gui-lib"
                     "scribble-lib"
                     "racket-doc"
                     "syntax-color-lib"))
(define deps '("base"))

(define pkg-desc "documentation part of \"syntax-color\"")

(define pkg-authors '(mflatt))
