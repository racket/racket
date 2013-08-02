#lang info

(define collection "unstable")

(define deps '("base"))
(define build-deps '("at-exp-lib"
                     "compatibility-lib"
                     "eli-tester"
                     "gui-lib"
                     "planet-lib"
                     "racket-test"
                     "rackunit-lib"
                     "scribble-lib"
                     "srfi-lib"
                     "syntax-color-lib"
                     "typed-racket-lib"
                     "unstable-contract-lib"
                     "unstable-debug-lib"
                     "unstable-lib"
                     "unstable-list-lib"
                     "unstable-options-lib"
                     "unstable-parameter-group-lib"))

(define pkg-desc "tests for \"unstable\"")

(define pkg-authors '(jay samth cce ryanc))
