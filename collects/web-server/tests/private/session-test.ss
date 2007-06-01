(module session-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  (provide session-tests)
  
  (define session-tests
    (test-suite
     "Sessions")))