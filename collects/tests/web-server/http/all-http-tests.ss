#lang scheme/base
(require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
         "cookies-test.ss"
         "digest-auth-test.ss")
(provide all-http-tests)

(define all-http-tests
  (test-suite
   "HTTP"
   cookies-tests
   digest-auth-tests))
