#lang scheme/base
(require rackunit
         macro-debugger/model/debug
         "gentest-framework.ss"
         "gentests.ss"
         "test-setup.ss"
         "tests/syntax-basic.ss"
         "tests/syntax-macros.ss"
         "tests/syntax-modules.ss"
         "tests/syntax-errors.ss"
         "tests/hiding.ss"
         "tests/regression.ss"
         "tests/policy.ss"
         ;;"tests/collects.ss"
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
