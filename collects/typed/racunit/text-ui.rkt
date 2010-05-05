#lang typed/scheme
(require typed/racunit
         typed/private/utils)

(define-type Verbosity
  (U 'quiet 'normal 'verbose))

(require/typed/provide
 racunit/text-ui
 [run-tests
  (case-lambda
    (Test -> Natural)
    (Test Verbosity -> Natural))])
(provide Verbosity)