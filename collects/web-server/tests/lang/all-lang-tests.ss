(module all-lang-tests mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           "abort-resume-test.ss"
           "anormal-test.ss"
           "file-box-test.ss"
           "labels-test.ss"
           "stuff-url-test.ss"
           "web-cells-test.ss"
           "web-extras-test.ss"
           "web-param-test.ss"
           "web-test.ss")
  (provide all-lang-tests)
  
  (define all-lang-tests  
    (test-suite
     "Web Language"
     abort-resume-tests
     anormal-tests
     file-box-tests
     labels-tests
     stuff-url-tests
     web-cells-tests
     web-extras-tests
     web-param-tests
     web-tests)))