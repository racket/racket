#lang racket/base
(require racket/cmdline
         rackunit
         rackunit/text-ui
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
         "tests/collects.rkt")
(provide all-tests)

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

(define-syntax-rule (with-namespace expr)
  (parameterize ((current-namespace (make-base-namespace)))
    expr))

;; ----

(define test-mode #f)
(define collects-tests? #f)

(command-line
 #:once-each
 [("--text") "Run tests in RackUnit text UI" (set! test-mode 'text)]
 [("--gui") "Run tests in RackUnit GUI" (set! test-mode 'gui)]
 [("--collects") "Include collects tests" (set! collects-tests? #t)]
 #:args ()
 (let* ([tests (cons all-tests (if collects-tests? (list collects-tests) null))])
   (case test-mode
     ((text)
      (with-namespace
       (for-each run-tests tests)))
     ((gui)
      (let ([test/gui (dynamic-require 'rackunit/gui 'test/gui)])
        (with-namespace
         (apply test/gui #:wait? #t tests)))))))
