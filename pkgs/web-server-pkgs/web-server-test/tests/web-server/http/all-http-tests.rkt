#lang racket/base
(require rackunit
         "cookies-test.rkt"
         "digest-auth-test.rkt")
(provide all-http-tests)

(define all-http-tests
  (test-suite
   "HTTP"
   cookies-tests
   digest-auth-tests))
