#lang racket
(require racunit
         web-server/private/connection-manager
         web-server/private/timer
         web-server/http/request
         web-server/http)
(provide request-tests)

(require/expose web-server/http/request
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
  (call-with-values (lambda () (read-bindings&post-data/raw conn #"POST" #f headers))
                    (lambda (f s) f)))

(define (get-post-data/raw post-data)
  (define-values (conn headers) (make-mock-connection&headers post-data))
  (call-with-values (lambda () (read-bindings&post-data/raw conn #"POST" #f headers))
                    (lambda (f s) s)))


(define request-tests
  (test-suite
   "HTTP Requests"
   
   (test-suite
    "Headers"
    (test-equal? "Simple" (header-value (headers-assq #"key" (list (make-header #"key" #"val")))) #"val")
    (test-false "Not present" (headers-assq #"key" (list)))
    (test-false "Case (not present)" (headers-assq* #"Key" (list)))
    (test-equal? "Case" (header-value (headers-assq* #"Key" (list (make-header #"key" #"val")))) #"val")
    (test-equal? "Case (not first)"
                 (header-value (headers-assq* #"Key" (list (make-header #"key1" #"val") (make-header #"key" #"val")))) #"val"))
   
   (test-suite
    "Bindings"
    (test-equal? "Simple" (binding:form-value (bindings-assq #"key" (list (make-binding:form #"key" #"val")))) #"val")
    (test-equal? "Simple (File)" (binding:file-content (bindings-assq #"key" (list (make-binding:file #"key" #"name" empty #"val")))) #"val")
    (test-false "Not present" (bindings-assq #"key" (list))))
   
   ; XXX This needs to be really extensive, see what Apache has
   (test-suite
    "Parsing"
    (test-suite
     "URL Query"
     (test-not-exn "Unfinished URL query"
                   (lambda ()
                     (define ip (open-input-string "GET http://127.0.0.1:8080/servlets/examples/hello.rkt?a=1&b: HTTP/1.1"))
                     (read-request
                      (make-connection 0 (make-timer ip +inf.0 (lambda () (void)))
                                       ip
                                       (open-output-bytes) (make-custodian) #f)
                      8081
                      (lambda _ (values "s1" "s2"))))))
    
    (test-suite
     "POST Bindings"
     (test-equal? "simple test 1"
                  (get-post-data/raw "hello world") #"hello world")
     (test-equal? "simple test 2"
                  (get-post-data/raw "hello=world") #"hello=world")
     (test-equal? "simple test 3"
                  (binding:form-value (bindings-assq #"hello" (get-bindings "hello=world")))
                  #"world")))))
