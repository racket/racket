(module dispatch-lang-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  (provide dispatch-lang-tests)
  
  ; XXX
  (define dispatch-lang-tests
    (test-suite
     "Web Language")))