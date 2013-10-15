#lang info

(define collection 'multi)

(define deps '("base"))

(define pkg-desc "RackUnit documentation")

(define pkg-authors '(noel ryanc))
(define build-deps '("racket-doc"
                     "rackunit-gui"
                     "rackunit-lib"
                     "scheme-lib"
                     "scribble-lib"))
