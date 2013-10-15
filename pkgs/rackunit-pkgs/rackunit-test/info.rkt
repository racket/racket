#lang info

(define collection 'multi)

(define deps '())

(define pkg-desc "RackUnit documentation")

(define pkg-authors '(noel ryanc))
(define build-deps '("base"
                     "eli-tester"
                     "rackunit-lib"
                     "srfi-lite-lib"))
