(module connection-manager-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  (provide connection-manager-tests)
  
  (define connection-manager-tests
    (test-suite
     "Connection Manager")))