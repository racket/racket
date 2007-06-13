(module web-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (lib "url.ss" "net")
           (lib "web.ss" "web-server" "servlet"))
  (provide web-tests)
  
  (define url0 (string->url "http://test.com/servlets/example.ss"))
  
  (define web-tests
    (test-suite
     "Web"
     
     (test-suite 
      "continuation-url? and embed-ids"
      (test-false "not k-url" (continuation-url? url0))
      (test-equal? "identity"
                   (continuation-url? (string->url (embed-ids (list 1 2 3) url0)))
                   (list 1 2 3))))))