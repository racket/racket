#lang info

(define collection 'multi)
(define deps '("unstable-debug-lib"
               "unstable-macro-testing-lib"
               "compiler-lib"
               "sandbox-lib"
               "compatibility-lib"
               "planet-lib"
               "net-lib"
               "net-test" ; for tests/net/available
               "serialize-cstruct-lib" ; tested here
               "eli-tester"
               ;; for `pkg` tests
               "web-server-lib"
               "rackunit-lib"
               ;; for `json` tests
               "at-exp-lib"

               "r5rs-lib"
               "scribble-lib"
               "syntax-color-lib"
               "typed-racket-lib"

               "cext-lib"

               ;; for random testing:
               "redex-lib"))

(define build-deps '("scheme-lib"
                     "srfi-lite-lib"
                     "base"
                     "data-lib"))

(define pkg-desc "Base Racket test suites")

(define pkg-authors '(eli jay matthias mflatt robby ryanc samth))
