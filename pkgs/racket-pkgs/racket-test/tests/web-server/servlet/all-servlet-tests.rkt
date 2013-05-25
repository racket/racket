#lang racket/base
(require rackunit
         "bindings-test.rkt"
         "basic-auth-test.rkt"
         "helpers-test.rkt"
         "web-test.rkt")
(provide all-servlet-tests)

(define all-servlet-tests
  (test-suite
   "Servlet (Internal)"
   bindings-tests
   basic-auth-tests
   helpers-tests
   web-tests))
