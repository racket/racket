#lang info

(define collection 'multi)
(define deps '("compiler-lib"
               "sandbox-lib"
               "compatibility-lib"
               "eli-tester"
               "planet-lib"
               "net-lib"
               "net-test" ; for tests/net/available
               "serialize-cstruct-lib" ; tested here
               "cext-lib" ; tested here
               "pconvert-lib" ; tested here

               ;; "racket-test-core" used to be part of this pkg
               "racket-test-core"

               ;; for `pkg` tests
               "web-server-lib"
               "rackunit-lib"

               ;; for `json` tests
               "at-exp-lib"

               ;; for contract tests
               "option-contract-lib"

               ;; used by the planet packages tested by the pkg tests
               "srfi-lib"

               ;; used to test setup, module readers, pkg system
               "scribble-lib"))

(define build-deps '("racket-index"
                     "scheme-lib"
                     "base"
                     "data-lib"))

(define implies '("racket-test-core"))

(define pkg-desc "Base Racket test suites")

(define pkg-authors '(eli jay matthias mflatt robby ryanc samth))
