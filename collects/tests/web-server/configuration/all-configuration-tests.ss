#lang scheme/base
(require (planet schematics/schemeunit:3)
         "configuration-table-test.ss")
(provide all-configuration-tests)

(define all-configuration-tests  
  (test-suite
   "Configuration"
   configuration-table-tests))
