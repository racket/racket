#lang scheme/base
(require schemeunit
         "configuration-table-test.ss")
(provide all-configuration-tests)

(define all-configuration-tests  
  (test-suite
   "Configuration"
   configuration-table-tests))
