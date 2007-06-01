(module cache-table-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  (provide cache-table-tests)
  
  (define cache-table-tests
    (test-suite
     "Cache Table")))