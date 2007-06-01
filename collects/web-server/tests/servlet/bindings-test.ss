(module bindings-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  (provide bindings-tests)
  
  (define bindings-tests
    (test-suite
     "Bindings")))