#lang scheme/base
(require rackunit
         macro-debugger/model/debug
         "gentest-framework.rkt"
         "gentests.rkt"
         "test-setup.rkt"
         "tests/syntax-basic.rkt"
         "tests/syntax-macros.rkt"
         "tests/syntax-modules.rkt"
         "tests/syntax-errors.rkt"
         "tests/hiding.rkt"
         "tests/regression.rkt"
         "tests/policy.rkt"
         ;;"tests/collects.rkt"
         )
(provide all-tests)

#|
(require rackunit/gui)
(define (go) (test/gui all-tests))
(define (collects) (test/gui big-libs-tests))
(provide go)
|#

(define protos
  (list proto:kernel-forms
        proto:kernel-contexts
        proto:macros
        proto:modules
        proto:errors))

(define deriv-test (mk-deriv-test protos))
(define steps-test (mk-steps-test protos))
(define hiding-deriv-test (mk-hidden-deriv-test protos))
(define hiding-steps-test (mk-hidden-steps-test protos))

(provide deriv-test
         steps-test
         hiding-deriv-test
         hiding-steps-test)

(define all-tests
  (test-suite "All tests"
    deriv-test
    steps-test
    hiding-deriv-test
    hiding-steps-test
    specialized-hiding-tests
    regression-tests
    policy-tests))
