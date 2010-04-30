#lang racket/base
(require racunit
         "cookies-test.rkt"
         "digest-auth-test.rkt")
(provide all-http-tests)

(define all-http-tests
  (test-suite
   "HTTP"
   cookies-tests
   digest-auth-tests))
