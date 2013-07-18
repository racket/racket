#lang info

(define collection 'multi)
(define deps '("unstable-debug-lib"
               "unstable-flonum-lib"
               "unstable-macro-testing-lib"
               "compiler-lib"
               "sandbox-lib"
               "compatibility-lib"
               "planet-lib"
               "distributed-places-lib"
               "pconvert-lib"
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
               "scribble-text-lib"
               "syntax-color-lib"
               "typed-racket-lib"
               "redex-lib"))

(define build-deps '("base"
                     "data-lib"))
