(module dispatch-files-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  (provide dispatch-files-tests)
  
  ; XXX
  (define dispatch-files-tests
    (test-suite
     "Files")))