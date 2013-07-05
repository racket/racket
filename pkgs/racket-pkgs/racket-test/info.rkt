#lang info

(define collection 'multi)
(define deps '("unstable-debug-lib"
               "unstable-flonum-lib"
               "compiler-lib"
               "sandbox-lib"
               "compatibility-lib"
               "pconvert-lib"
               ;; for `net` tests
               "net-lib"
               ;; for `pkg` tests
               "web-server-lib"
               "rackunit-lib"
               ;; for `json` tests
               "at-exp-lib"))
