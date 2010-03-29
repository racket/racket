#lang scheme/base

(require schemeunit
         schemeunit/text-ui
         "all-schemeunit-tests.ss")

(run-tests all-schemeunit-tests)

;; Don't run the failing tests by default. Switch the comments if you want to inspect the visual appearance of failing test's output.
;(run-tests success-and-failure-tests)
