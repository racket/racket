(module dispatch-lang-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  (provide dispatch-lang-tests)
  
  (define dispatch-lang-tests
    (test-suite
     "Web Language"
     
     ; XXX test web.ss
     (test-suite
      "web.ss")

     ; XXX test web-extras.ss
     (test-suite
      "web-extras.ss")
     
     ; XXX test web-cells.ss
     (test-suite
      "web-cell.ss")
     )))