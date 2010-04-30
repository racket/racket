#lang racket/base
(require rktunit
         "configuration-table-test.rkt")
(provide all-configuration-tests)

(define all-configuration-tests  
  (test-suite
   "Configuration"
   configuration-table-tests))
