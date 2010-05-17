#lang racket/base
(require rackunit
         "dispatch-passwords-test.rkt"
         "dispatch-files-test.rkt"        
         "dispatch-servlets-test.rkt"
         "dispatch-lang-test.rkt"         
         "dispatch-host-test.rkt"
         "filesystem-map-test.rkt")
(provide all-dispatchers-tests)

(define all-dispatchers-tests  
  (test-suite
   "Dispatchers"
   dispatch-passwords-tests
   dispatch-host-tests
   dispatch-files-tests
   dispatch-servlets-tests
   dispatch-lang-tests
   filesystem-map-tests))
