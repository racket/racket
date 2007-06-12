(module web-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (lib "web.ss" "web-server" "servlet"))
  (provide web-tests)
  
  (define web-tests
    (test-suite
     "Web"
     
     (test-suite 
      "continuation-url?"
      )
     
     (test-suite
      "embed-ids"
      )
     
     (test-suite
      "xexpr/callback->xexpr"
      ))))