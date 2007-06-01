(module all-servlet-tests mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           "bindings-test.ss"
           "servlet-url-test.ss"
           "basic-auth-test.ss"
           "helpers-test.ss"
           "web-test.ss")
  (provide all-servlet-tests)
  
  (define all-servlet-tests
    (test-suite
     "Servlet (Internal)"
     bindings-tests
     servlet-url-tests
     basic-auth-tests
     helpers-tests
     web-tests)))