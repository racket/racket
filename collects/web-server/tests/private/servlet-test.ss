(module servlet-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (lib "servlet.ss" "web-server"))
  (provide servlet-tests)
  
  ; XXX
  (define servlet-tests
    (test-suite
     "Servlets")))