#lang info

(define collection 'multi)

(define deps '("base"))

(define pkg-desc "RackUnit documentation")

(define pkg-authors '(noel ryanc))
(define build-deps '("racket-index"
                     "racket-doc"
                     "rackunit-gui"
                     "rackunit-lib"
                     "scribble-lib"))
(define update-implies '("rackunit-lib"))
