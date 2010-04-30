#lang racket/base
(require racunit
         "configuration-table-test.rkt")
(provide all-configuration-tests)

(define all-configuration-tests  
  (test-suite
   "Configuration"
   configuration-table-tests))
