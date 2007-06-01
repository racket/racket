(module web-cells-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  (provide web-cells-tests)
  
  (define web-cells-tests
    (test-suite
     "Web Cells")))