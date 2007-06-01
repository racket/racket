(module all-dispatchers-tests mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  (provide all-dispatchers-tests)
  
  (define all-dispatchers-tests  
    (test-suite
     "Dispatchers")))