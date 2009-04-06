#lang scheme/base
(require schemeunit
         "cookies-test.ss"
         "digest-auth-test.ss")
(provide all-http-tests)

(define all-http-tests
  (test-suite
   "HTTP"
   cookies-tests
   digest-auth-tests))
