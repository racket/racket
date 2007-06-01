(module basic-auth-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  (provide basic-auth-tests)
  
  ; XXX
  (define basic-auth-tests
    (test-suite
     "BASIC Authentication")))