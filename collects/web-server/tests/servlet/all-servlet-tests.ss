(module all-servlet-tests mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  (provide all-servlet-tests)
  
  (define all-servlet-tests
    (test-suite
     "Servlet (Internal)")))