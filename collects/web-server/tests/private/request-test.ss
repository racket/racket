(module request-test mzscheme
  (require (planet "util.ss" ("schematics" "schemeunit.plt" 2))
           (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (lib "connection-structs.ss" "web-server" "private")
           (lib "timer-structs.ss" "web-server" "private")
           (lib "request-structs.ss" "web-server" "private"))
  
  (require/expose (lib "request.ss" "web-server" "private")
                  (read-bindings&post-data/raw))
  
  ;; mock connection object for test on post body parsing
  (define (make-mock-connection&headers post-body)
    (let* ([bytes (string->bytes/utf-8 post-body)]
           [headers (list (make-header
                           #"Content-Length"
                           (string->bytes/utf-8
                            (number->string (bytes-length bytes)))))]
           [ip (open-input-bytes bytes)]
           [op (open-output-bytes)])
      (values (make-connection (make-timer ip +inf.0 (lambda () (void)))
                               ip
                               op
                               (make-custodian)
                               #f
                               (make-semaphore))
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
     "tests for parsing bindings"
     (test-equal? "simple test 1"
                  (get-post-data/raw "hello world") #"hello world")
     (test-equal? "simple test 2"
                  (get-post-data/raw "hello=world") #"hello=world")
     (test-equal? "simple test 3"
                  (binding:form-value (bindings-assq #"hello" (get-bindings "hello=world")))
                  #"world")))
  
  (provide request-tests))