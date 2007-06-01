(module web-extras-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  (provide web-extras-tests)
  
  ; XXX
  (define web-extras-tests
    (test-suite
     "Web Extras")))