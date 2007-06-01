(module session-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  (provide session-tests)
  
  ; XXX
  (define session-tests
    (test-suite
     "Sessions")))