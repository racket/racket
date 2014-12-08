#lang info

(define collection 'multi)
(define deps '("unstable-macro-testing-lib"
               "compiler-lib"
               "sandbox-lib"
               "compatibility-lib"
               "eli-tester"
               "planet-lib"
               "net-lib"
               "net-test" ; for tests/net/available
               "serialize-cstruct-lib" ; tested here               
               "cext-lib" ; tested here
               "pconvert-lib" ; tested here

               ;; for `pkg` tests
               "web-server-lib"
               "rackunit-lib"

               ;; for `json` tests
               "at-exp-lib"

               ;; used to test setup, module readers, pkg system
               "scribble-lib"

               ;; for random testing:
               "redex-lib"))

(define build-deps '("racket-index"
                     "scheme-lib"
                     "srfi-lite-lib"
                     "base"
                     "data-lib"))

(define pkg-desc "Base Racket test suites")

(define pkg-authors '(eli jay matthias mflatt robby ryanc samth))
