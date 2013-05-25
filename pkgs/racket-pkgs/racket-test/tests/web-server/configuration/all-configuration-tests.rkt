#lang racket/base
(require rackunit
         "configuration-table-test.rkt")
(provide all-configuration-tests)

(define all-configuration-tests  
  (test-suite
   "Configuration"
   configuration-table-tests))
