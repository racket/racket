(module mod-map-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  (provide mod-map-tests)
  
  (define mod-map-tests
    (test-suite
     "Module Map")))