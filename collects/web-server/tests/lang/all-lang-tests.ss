(module all-lang-tests mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           "anormal-test.ss"
           "labels-test.ss"
           "stuff-url-test.ss"
           "web-param-test.ss")
  (provide all-lang-tests)
  
  (define all-lang-tests  
    (test-suite
     "Web Language"
     anormal-tests
     labels-tests
     stuff-url-tests
     web-param-tests)))