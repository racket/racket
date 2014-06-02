#lang info

(define collection 'multi)

(define deps '("base"))
(define build-deps '("racket-index"
                     "scheme-lib"
                     "at-exp-lib"
                     "compatibility-lib"
                     "eli-tester"
                     "gui-lib"
                     "planet-lib"
                     "racket-test"
                     "rackunit-lib"
                     "srfi-lib"
                     "syntax-color-lib"
                     "typed-racket-lib"
                     "unstable-contract-lib"
                     "unstable-debug-lib"
                     "unstable-lib"
                     "unstable-list-lib"
                     "unstable-options-lib"
                     "unstable-parameter-group-lib"
                     "unstable-2d"))

(define pkg-desc "tests for \"unstable\"")

(define pkg-authors '(jay samth cce ryanc robby))
