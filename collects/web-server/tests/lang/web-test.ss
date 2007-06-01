(module web-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  (provide web-tests)
  
  (define web-tests
    (test-suite
     "Web")))