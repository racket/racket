(module servlet-url-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  (provide servlet-url-tests)
  
  ; XXX
  (define servlet-url-tests
    (test-suite
     "Servlet URLs")))