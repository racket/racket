(module bindings-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (lib "list.ss")
           (lib "url.ss" "net")
           (lib "request-structs.ss" "web-server" "private")
           (lib "bindings.ss" "web-server" "servlet"))
  (provide bindings-tests)
  
  (define bs `([foo . 3] [foos . 1] [foos . 2]))
  
  (define bindings-tests
    (test-suite
     "Bindings"
     
     (test-suite
      "request-bindings"
      (test-case
       "Simple"
       (check-equal? (request-bindings
                      (make-request 'get (string->url "http://test.com/foo")
                                    empty (list (make-binding:form #"key" #"val")) #f
                                    "host" 80 "client"))
                     '((key . "val"))))
      (test-case
       "Case"
       (check-equal? (request-bindings
                      (make-request 'get (string->url "http://test.com/foo")
                                    empty (list (make-binding:form #"KEY" #"val")) #f
                                    "host" 80 "client"))
                     '((key . "val"))))
      (test-case
       "Multi"
       (check-equal? (request-bindings
                      (make-request 'get (string->url "http://test.com/foo")
                                    empty (list (make-binding:form #"key" #"val")
                                                (make-binding:form #"key2" #"val")) #f
                                                                                    "host" 80 "client"))
                     '((key . "val")
                       (key2 . "val"))))
      (test-case
       "File"
       (check-equal? (request-bindings
                      (make-request 'get (string->url "http://test.com/foo")
                                    empty (list (make-binding:file #"key" #"file" #"val")) #f
                                    "host" 80 "client"))
                     '((key . #"val")))))
     
     (test-suite
      "request-headers"
      (test-case
       "Simple"
       (check-equal? (request-headers
                      (make-request 'get (string->url "http://test.com/foo")
                                    (list (make-header #"key" #"val")) empty #f
                                    "host" 80 "client"))
                     '((key . "val"))))
      (test-case
       "Case"
       (check-equal? (request-headers
                      (make-request 'get (string->url "http://test.com/foo")
                                    (list (make-header #"KEY" #"val")) empty #f
                                    "host" 80 "client"))
                     '((key . "val")))))
     
     (test-case
      "exists-binding? - true"
      (check-true (exists-binding? 'foo bs)))
     (test-case
      "exists-binding? - false"
      (check-false (exists-binding? 'bar bs)))
     
     (test-case
      "extract-bindings"
      (check-equal? (extract-bindings 'foos bs) (list 1 2)))
     
     (test-case
      "extract-binding/single - success"
      (check-equal? (extract-binding/single 'foo bs) 3))
     (test-case
      "extract-binding/single - failure"
      (check-exn (lambda (exn) (regexp-match "not found" (exn-message exn)))
                 (lambda () (extract-binding/single 'bar bs) 3)))
     (test-case
      "extract-binding/single - multiple"
      (check-exn (lambda (exn) (regexp-match "multiple times" (exn-message exn)))
                 (lambda () (extract-binding/single 'foos bs) 3))))))