(module all-private-tests mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           "define-closure-test.ss"
           "request-test.ss")
  (provide all-private-tests)
  
  (define all-private-tests
    (test-suite
     "Internal"
     define-closure-tests
     request-tests)))