(module request-test mzscheme
  (require (planet "util.ss" ("schematics" "schemeunit.plt" 2))
           (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (lib "connection-manager.ss" "web-server" "private")
           (lib "timer.ss" "web-server" "private")
           (lib "request-structs.ss" "web-server" "private"))
  (provide request-tests)
  
  (require/expose (lib "request.ss" "web-server" "private")
                  (read-bindings&post-data/raw))
  
  ;; mock connection object for test on post body parsing
  (define (make-mock-connection&headers post-body)
    (let* ([b (string->bytes/utf-8 post-body)]
           [headers (list (make-header
                           #"Content-Length"
                           (string->bytes/utf-8
                            (number->string (bytes-length b)))))]
           [ip (open-input-bytes b)]
           [op (open-output-bytes)])
      (values (make-connection 0 (make-timer ip +inf.0 (lambda () (void)))
                               ip op (make-custodian) #f)
              headers)))
  
  (define (get-bindings post-data)
    (define-values (conn headers) (make-mock-connection&headers post-data))
    (call-with-values (lambda () (read-bindings&post-data/raw conn 'post #f headers))
                      (lambda (f s) f)))
  
  (define (get-post-data/raw post-data)
    (define-values (conn headers) (make-mock-connection&headers post-data))
    (call-with-values (lambda () (read-bindings&post-data/raw conn 'post #f headers))
                      (lambda (f s) s)))
  
  
  (define request-tests
    (test-suite
     "HTTP Requests"
     
     (test-suite
      "Headers"
      (test-equal? "Simple" (header-value (headers-assq #"key" (list (make-header #"key" #"val")))) #"val")
      (test-false "Not present" (headers-assq #"key" (list)))
      (test-equal? "Case" (header-value (headers-assq* #"Key" (list (make-header #"key" #"val")))) #"val"))
     
     (test-suite
      "Bindings"
      (test-equal? "Simple" (binding:form-value (bindings-assq #"key" (list (make-binding:form #"key" #"val")))) #"val")
      (test-equal? "Simple (File)" (binding:file-content (bindings-assq #"key" (list (make-binding:file #"key" #"name" #"val")))) #"val")
      (test-false "Not present" (bindings-assq #"key" (list))))
     
     ; XXX This needs to be really extensive, see what Apache has
     (test-suite
      "Parsing"
      (test-suite
       "POST Bindings"
       (test-equal? "simple test 1"
                    (get-post-data/raw "hello world") #"hello world")
       (test-equal? "simple test 2"
                    (get-post-data/raw "hello=world") #"hello=world")
       (test-equal? "simple test 3"
                    (binding:form-value (bindings-assq #"hello" (get-bindings "hello=world")))
                    #"world"))))))