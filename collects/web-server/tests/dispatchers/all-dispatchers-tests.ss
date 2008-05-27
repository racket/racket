#lang scheme/base
(require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
         "dispatch-passwords-test.ss"
         "dispatch-files-test.ss"        
         "dispatch-servlets-test.ss"
         "dispatch-lang-test.ss"         
         "dispatch-host-test.ss"
         "filesystem-map-test.ss")
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
