(module util-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  (provide util-tests)
  
  (define util-tests
    (test-suite
     "Utilities")))