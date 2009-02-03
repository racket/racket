#lang scheme/base
(require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
         "cookies-test.ss")
(provide all-http-tests)

(define all-http-tests
  (test-suite
   "HTTP"
   cookies-tests))
