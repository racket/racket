(module mime-types-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  (provide mime-types-tests)
  
  (define mime-types-tests
    (test-suite
     "MIME Types")))