#lang info

(define collection 'multi)
(define deps '("unstable-debug-lib"
               "unstable-macro-testing-lib"
               "compiler-lib"
               "sandbox-lib"
               "compatibility-lib"
               "planet-lib"
               "distributed-places-lib"
               "eli-tester"
               ;; for `net` tests
               "net-lib"
               ;; for `pkg` tests
               "web-server-lib"
               "rackunit-lib"
               ;; for `json` tests
               "at-exp-lib"

               "r5rs-lib"
               "scribble-lib"
               "syntax-color-lib"
               "typed-racket-lib"
               "serialize-cstruct-lib"

               "cext-lib"

               ;; for random testing:
               "redex-lib"))

(define build-deps '("scheme-lib"
                     "srfi-lite-lib"
                     "base"
                     "data-lib"))

(define pkg-desc "Base Racket test suites")

(define pkg-authors '(eli jay matthias mflatt robby ryanc samth))
