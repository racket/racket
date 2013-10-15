#lang info

(define collection 'multi)

(define deps '("base"))

(define pkg-desc "tests for \"planet-lib\"")

(define pkg-authors '(mflatt))
(define build-deps '("eli-tester"
                     "planet-lib"
                     "rackunit-lib"
                     "scheme-lib"
                     "scribble-lib"))
