(module response-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (lib "response.ss" "web-server" "private")
           (lib "response-structs.ss" "web-server" "private")
           (lib "connection-manager.ss" "web-server" "private")
           (lib "timer.ss" "web-server" "private"))
  (provide response-tests)
  
  (define (make-mock-connection ib)
    (define ip (open-input-bytes ib))
    (define op (open-output-bytes))
    (values (make-connection (make-timer never-evt +inf.0 (lambda () (void)))
                             ip op (make-custodian) #f (make-semaphore 1))
            ip
            op))
  
  (define (output f . any)
    (define-values (c i o) (make-mock-connection #""))
    (apply f c any)
    (regexp-replace 
     #"Date: [a-zA-Z0-9:, ]+ GMT\r\n"
     (regexp-replace
      #"Last-Modified: [a-zA-Z0-9:, ]+ GMT\r\n"
      (get-output-bytes o)
      #"Last-Modified: XXX GMT\r\n")
     #"Date: XXX GMT\r\n"))
  
  ; XXX
  (define response-tests
    (test-suite
     "HTTP Responses"
     
     (test-suite
      "output-response"
      (test-suite 
       "response/full"
       (test-equal? "response/full" 
                    (output output-response 
                            (make-response/full 404 "404" (current-seconds) #"text/html"
                                                (list) (list)))
                    #"HTTP/1.1 404 404\r\nDate: XXX GMT\r\nLast-Modified: XXX GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nContent-Length: 0\r\n\r\n")
       (test-equal? "response/full (header)" 
                    (output output-response 
                            (make-response/full 404 "404" (current-seconds) #"text/html"
                                                (list (cons 'Header "Value")) (list)))
                    #"HTTP/1.1 404 404\r\nDate: XXX GMT\r\nLast-Modified: XXX GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nContent-Length: 0\r\nHeader: Value\r\n\r\n")
       (test-equal? "response/full (body)" 
                    (output output-response 
                            (make-response/full 404 "404" (current-seconds) #"text/html"
                                                (list) (list "Content!")))
                    #"HTTP/1.1 404 404\r\nDate: XXX GMT\r\nLast-Modified: XXX GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nContent-Length: 8\r\n\r\nContent!")
       (test-equal? "response/full (bytes body)"
                    (output output-response 
                            (make-response/full 404 "404" (current-seconds) #"text/html"
                                                (list) (list #"Content!")))
                    #"HTTP/1.1 404 404\r\nDate: XXX GMT\r\nLast-Modified: XXX GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nContent-Length: 8\r\n\r\nContent!")
       (test-equal? "response/full (both)" 
                    (output output-response 
                            (make-response/full 404 "404" (current-seconds) #"text/html"
                                                (list (cons 'Header "Value")) (list "Content!")))
                    #"HTTP/1.1 404 404\r\nDate: XXX GMT\r\nLast-Modified: XXX GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nContent-Length: 8\r\nHeader: Value\r\n\r\nContent!"))
      (test-suite
       "response/incremental"
       (test-equal? "response/incremental" 
                    (output output-response 
                            (make-response/incremental 404 "404" (current-seconds) #"text/html"
                                                       (list) (lambda (write) (void))))
                    #"HTTP/1.1 404 404\r\nDate: XXX GMT\r\nLast-Modified: XXX GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nTransfer-Encoding: chunked\r\n\r\n0\r\n\r\n")
       (test-equal? "response/incremental (header)" 
                    (output output-response 
                            (make-response/incremental 404 "404" (current-seconds) #"text/html"
                                                       (list (cons 'Header "Value"))
                                                       (lambda (write) (void))))
                    #"HTTP/1.1 404 404\r\nDate: XXX GMT\r\nLast-Modified: XXX GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nTransfer-Encoding: chunked\r\nHeader: Value\r\n\r\n0\r\n\r\n")
       (test-equal? "response/incremental (body)" 
                    (output output-response 
                            (make-response/incremental 404 "404" (current-seconds) #"text/html"
                                                       (list) 
                                                       (lambda (write) (write "Content!"))))
                    #"HTTP/1.1 404 404\r\nDate: XXX GMT\r\nLast-Modified: XXX GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nTransfer-Encoding: chunked\r\n\r\n8\r\nContent!\r\n0\r\n\r\n")
       (test-equal? "response/incremental (bytes body)"
                    (output output-response 
                            (make-response/incremental 404 "404" (current-seconds) #"text/html"
                                                       (list) 
                                                       (lambda (write) (write #"Content!"))))
                    #"HTTP/1.1 404 404\r\nDate: XXX GMT\r\nLast-Modified: XXX GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nTransfer-Encoding: chunked\r\n\r\n8\r\nContent!\r\n0\r\n\r\n")
       (test-equal? "response/incremental (both)" 
                    (output output-response 
                            (make-response/incremental 404 "404" (current-seconds) #"text/html"
                                                       (list (cons 'Header "Value"))
                                                       (lambda (write) (write "Content!"))))
                    #"HTTP/1.1 404 404\r\nDate: XXX GMT\r\nLast-Modified: XXX GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nTransfer-Encoding: chunked\r\nHeader: Value\r\n\r\n8\r\nContent!\r\n0\r\n\r\n")
       (test-equal? "response/incremental (twice)" 
                    (output output-response 
                            (make-response/incremental 404 "404" (current-seconds) #"text/html"
                                                       (list (cons 'Header "Value"))
                                                       (lambda (write) 
                                                         (write "Content!")
                                                         (write "Content!"))))
                    #"HTTP/1.1 404 404\r\nDate: XXX GMT\r\nLast-Modified: XXX GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nTransfer-Encoding: chunked\r\nHeader: Value\r\n\r\n8\r\nContent!\r\n8\r\nContent!\r\n0\r\n\r\n"))
      (test-suite
       "Simple content"
       (test-equal? "empty"
                    (output output-response
                            (list #"text/html"))
                    #"HTTP/1.1 200 Okay\r\nDate: XXX GMT\r\nLast-Modified: XXX GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nContent-Length: 0\r\n\r\n")
       (test-equal? "not"
                    (output output-response
                            (list #"text/html" "Content"))
                    #"HTTP/1.1 200 Okay\r\nDate: XXX GMT\r\nLast-Modified: XXX GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nContent-Length: 7\r\n\r\nContent")
       (test-equal? "not, bytes"
                    (output output-response
                            (list #"text/html" #"Content"))
                    #"HTTP/1.1 200 Okay\r\nDate: XXX GMT\r\nLast-Modified: XXX GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nContent-Length: 7\r\n\r\nContent"))
      (test-suite
       "xexpr"
       (test-equal? "any"
                    (output output-response
                            `(html (head (title "Hey!")) (body "Content")))
                    #"HTTP/1.1 200 Okay\r\nDate: XXX GMT\r\nLast-Modified: XXX GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html; charset=utf-8\r\nContent-Length: 66\r\n\r\n<html><head><title>Hey!</title></head><body>Content</body></html>\n")))
     ; XXX
     (test-suite
      "output-response/method")
     ; XXX
     (test-suite
      "output-file"))))