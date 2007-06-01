(module response-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  (provide response-tests)
  
  (define response-tests
    (test-suite
     "HTTP Responses")))