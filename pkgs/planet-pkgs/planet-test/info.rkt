#lang info

(define collection 'multi)

(define deps '("base"))

(define pkg-desc "tests for \"planet-lib\"")

(define pkg-authors '(mflatt robby))
(define build-deps '("eli-tester"
                     "planet-lib"
                     "rackunit-lib"
                     "scheme-lib"
                     "scribble-lib"))

(define test-omit-paths
  '("tests/planet/examples"))
