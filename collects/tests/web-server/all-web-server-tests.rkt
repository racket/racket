#lang racket/base
(require rackunit
         "configuration/all-configuration-tests.rkt"
         "dispatchers/all-dispatchers-tests.rkt"           
         "lang/all-lang-tests.rkt"
         "lang-test.rkt"
         "managers/all-managers-tests.rkt"
         "http/all-http-tests.rkt"
         "private/all-private-tests.rkt"
         "servlet/all-servlet-tests.rkt"
         "stuffers-test.rkt"
         "formlets-test.rkt"
         "dispatch-test.rkt"
         "servlet-env-test.rkt"
         "test-tests.rkt")
(provide all-web-server-tests)

(define all-web-server-tests  
  (test-suite
   "Web Server"
   all-http-tests
   all-stuffers-tests
   all-formlets-tests
   all-dispatch-tests
   all-configuration-tests
   all-dispatchers-tests
   all-lang-tests
   lang-tests
   all-managers-tests
   all-private-tests
   all-servlet-tests
   servlet-env-tests
   test-tests))
