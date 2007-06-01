(module file-box-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  (provide file-box-tests)
  
  (define file-box-tests
    (test-suite
     "File Boxes")))