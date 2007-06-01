(module configuration-table-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  (provide configuration-table-tests)
  
  (define configuration-table-tests
    (test-suite
     "Configuration Table")))