(module response-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (lib "xml.ss" "xml")
           (lib "file.ss")
           (lib "response.ss" "web-server" "private")
           (lib "request-structs.ss" "web-server" "private")
           (lib "response-structs.ss" "web-server" "private")
           "../util.ss")
  (provide response-tests)
    
  (define (output f . any)
    (define-values (c i o) (make-mock-connection #""))
    (apply f c any)
    (redact (get-output-bytes o)))
  
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
                    #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nContent-Length: 0\r\n\r\n")
       (test-equal? "response/full (header)" 
                    (output output-response 
                            (make-response/full 404 "404" (current-seconds) #"text/html"
                                                (list (make-header #"Header" #"Value")) (list)))
                    #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nContent-Length: 0\r\nHeader: Value\r\n\r\n")
       (test-equal? "response/full (body)" 
                    (output output-response 
                            (make-response/full 404 "404" (current-seconds) #"text/html"
                                                (list) (list "Content!")))
                    #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nContent-Length: 8\r\n\r\nContent!")
       (test-equal? "response/full (bytes body)"
                    (output output-response 
                            (make-response/full 404 "404" (current-seconds) #"text/html"
                                                (list) (list #"Content!")))
                    #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nContent-Length: 8\r\n\r\nContent!")
       (test-equal? "response/full (both)" 
                    (output output-response 
                            (make-response/full 404 "404" (current-seconds) #"text/html"
                                                (list (make-header #"Header" #"Value")) (list "Content!")))
                    #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nContent-Length: 8\r\nHeader: Value\r\n\r\nContent!"))
      (test-suite
       "response/incremental"
       (test-equal? "response/incremental" 
                    (output output-response 
                            (make-response/incremental 404 "404" (current-seconds) #"text/html"
                                                       (list) (lambda (write) (void))))
                    #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nTransfer-Encoding: chunked\r\n\r\n0\r\n\r\n")
       (test-equal? "response/incremental (header)" 
                    (output output-response 
                            (make-response/incremental 404 "404" (current-seconds) #"text/html"
                                                       (list (make-header #"Header" #"Value"))
                                                       (lambda (write) (void))))
                    #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nTransfer-Encoding: chunked\r\nHeader: Value\r\n\r\n0\r\n\r\n")
       (test-equal? "response/incremental (body)" 
                    (output output-response 
                            (make-response/incremental 404 "404" (current-seconds) #"text/html"
                                                       (list) 
                                                       (lambda (write) (write "Content!"))))
                    #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nTransfer-Encoding: chunked\r\n\r\n8\r\nContent!\r\n0\r\n\r\n")
       (test-equal? "response/incremental (bytes body)"
                    (output output-response 
                            (make-response/incremental 404 "404" (current-seconds) #"text/html"
                                                       (list) 
                                                       (lambda (write) (write #"Content!"))))
                    #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nTransfer-Encoding: chunked\r\n\r\n8\r\nContent!\r\n0\r\n\r\n")
       (test-equal? "response/incremental (both)" 
                    (output output-response 
                            (make-response/incremental 404 "404" (current-seconds) #"text/html"
                                                       (list (make-header #"Header" #"Value"))
                                                       (lambda (write) (write "Content!"))))
                    #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nTransfer-Encoding: chunked\r\nHeader: Value\r\n\r\n8\r\nContent!\r\n0\r\n\r\n")
       (test-equal? "response/incremental (twice)" 
                    (output output-response 
                            (make-response/incremental 404 "404" (current-seconds) #"text/html"
                                                       (list (make-header #"Header" #"Value"))
                                                       (lambda (write) 
                                                         (write "Content!")
                                                         (write "Content!"))))
                    #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nTransfer-Encoding: chunked\r\nHeader: Value\r\n\r\n8\r\nContent!\r\n8\r\nContent!\r\n0\r\n\r\n"))
      (test-suite
       "Simple content"
       (test-equal? "empty"
                    (output output-response
                            (list #"text/html"))
                    #"HTTP/1.1 200 Okay\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nContent-Length: 0\r\n\r\n")
       (test-equal? "not"
                    (output output-response
                            (list #"text/html" "Content"))
                    #"HTTP/1.1 200 Okay\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nContent-Length: 7\r\n\r\nContent")
       (test-equal? "not, bytes"
                    (output output-response
                            (list #"text/html" #"Content"))
                    #"HTTP/1.1 200 Okay\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nContent-Length: 7\r\n\r\nContent"))
      (test-suite
       "xexpr"
       (test-equal? "any"
                    (output output-response
                            `(html (head (title "Hey!")) (body "Content")))
                    #"HTTP/1.1 200 Okay\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html; charset=utf-8\r\nContent-Length: 65\r\n\r\n<html><head><title>Hey!</title></head><body>Content</body></html>")))
     (test-suite
      "output-response/method"
      (test-suite 
       "response/full"
       (test-equal? "response/full" 
                    (output output-response/method 
                            (make-response/full 404 "404" (current-seconds) #"text/html"
                                                (list) (list))
                            'head)
                    #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nContent-Length: 0\r\n\r\n")
       (test-equal? "response/full (header)" 
                    (output output-response/method
                            (make-response/full 404 "404" (current-seconds) #"text/html"
                                                (list (make-header #"Header" #"Value")) (list))
                            'head)
                    #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nContent-Length: 0\r\nHeader: Value\r\n\r\n")
       (test-equal? "response/full (body)" 
                    (output output-response/method
                            (make-response/full 404 "404" (current-seconds) #"text/html"
                                                (list) (list "Content!"))
                            'head)
                    #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nContent-Length: 8\r\n\r\n")
       (test-equal? "response/full (bytes body)"
                    (output output-response/method 
                            (make-response/full 404 "404" (current-seconds) #"text/html"
                                                (list) (list #"Content!"))
                            'head)
                    #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nContent-Length: 8\r\n\r\n")
       (test-equal? "response/full (both)" 
                    (output output-response/method
                            (make-response/full 404 "404" (current-seconds) #"text/html"
                                                (list (make-header #"Header" #"Value")) (list "Content!"))
                            'head)
                    #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nContent-Length: 8\r\nHeader: Value\r\n\r\n"))
      (test-suite
       "response/incremental"
       (test-equal? "response/incremental" 
                    (output output-response/method 
                            (make-response/incremental 404 "404" (current-seconds) #"text/html"
                                                       (list) (lambda (write) (void)))
                            'head)
                    #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nTransfer-Encoding: chunked\r\n\r\n")
       (test-equal? "response/incremental (header)" 
                    (output output-response/method
                            (make-response/incremental 404 "404" (current-seconds) #"text/html"
                                                       (list (make-header #"Header" #"Value"))
                                                       (lambda (write) (void)))
                            'head)
                    #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nTransfer-Encoding: chunked\r\nHeader: Value\r\n\r\n")
       (test-equal? "response/incremental (body)" 
                    (output output-response/method
                            (make-response/incremental 404 "404" (current-seconds) #"text/html"
                                                       (list) 
                                                       (lambda (write) (write "Content!")))
                            'head)
                    #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nTransfer-Encoding: chunked\r\n\r\n")
       (test-equal? "response/incremental (bytes body)"
                    (output output-response/method
                            (make-response/incremental 404 "404" (current-seconds) #"text/html"
                                                       (list) 
                                                       (lambda (write) (write #"Content!")))
                            'head)
                    #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nTransfer-Encoding: chunked\r\n\r\n")
       (test-equal? "response/incremental (both)" 
                    (output output-response/method
                            (make-response/incremental 404 "404" (current-seconds) #"text/html"
                                                       (list (make-header #"Header" #"Value"))
                                                       (lambda (write) (write "Content!")))
                            'head)
                    #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nTransfer-Encoding: chunked\r\nHeader: Value\r\n\r\n")
       (test-equal? "response/incremental (twice)" 
                    (output output-response/method
                            (make-response/incremental 404 "404" (current-seconds) #"text/html"
                                                       (list (make-header #"Header" #"Value"))
                                                       (lambda (write) 
                                                         (write "Content!")
                                                         (write "Content!")))
                            'head)
                    #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nTransfer-Encoding: chunked\r\nHeader: Value\r\n\r\n"))
      (test-suite
       "Simple content"
       (test-equal? "empty"
                    (output output-response/method
                            (list #"text/html")
                            'head)
                    #"HTTP/1.1 200 Okay\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nContent-Length: 0\r\n\r\n")
       (test-equal? "not"
                    (output output-response/method
                            (list #"text/html" "Content")
                            'head)
                    #"HTTP/1.1 200 Okay\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nContent-Length: 7\r\n\r\n")
       (test-equal? "not, bytes"
                    (output output-response/method
                            (list #"text/html" #"Content")
                            'head)
                    #"HTTP/1.1 200 Okay\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nContent-Length: 7\r\n\r\n"))
      (test-suite
       "xexpr"
       (test-equal? "any"
                    (output output-response/method
                            `(html (head (title "Hey!")) (body "Content"))
                            'head)
                    #"HTTP/1.1 200 Okay\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html; charset=utf-8\r\nContent-Length: 65\r\n\r\n")))
     (let ()
       (define tmp-file (make-temporary-file))
       (with-output-to-file tmp-file 
         (lambda ()
           (display
            (xexpr->string 
             `(html (head (title "A title"))
                    (body "Here's some content!")))))
         'truncate/replace)
       (test-suite
        "output-file"
        (test-equal? "(get) whole-file"
                     (output output-file tmp-file 'get #"text/html"
                             0 +inf.0)
                     #"HTTP/1.1 206 Okay\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nContent-Length: 81\r\nContent-Range: bytes 0-81/81\r\n\r\n<html><head><title>A title</title></head><body>Here's some content!</body></html>")
        (test-equal? "(get) end early"
                     (output output-file tmp-file 'get #"text/html"
                             0 10)
                     #"HTTP/1.1 206 Okay\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nContent-Length: 10\r\nContent-Range: bytes 0-10/81\r\n\r\n<html><hea")
        (test-equal? "(get) start late"
                     (output output-file tmp-file 'get #"text/html"
                             10 +inf.0)
                     #"HTTP/1.1 206 Okay\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nContent-Length: 71\r\nContent-Range: bytes 10-81/81\r\n\r\nd><title>A title</title></head><body>Here's some content!</body></html>")
        (test-equal? "(get) start late and end early"
                     (output output-file tmp-file 'get #"text/html"
                             5 10)
                     #"HTTP/1.1 206 Okay\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nContent-Length: 5\r\nContent-Range: bytes 5-10/81\r\n\r\n><head><ti")
        (test-equal? "(head) whole-file"
                     (output output-file tmp-file 'head #"text/html"
                             0 +inf.0)
                     #"HTTP/1.1 206 Okay\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nContent-Length: 81\r\nContent-Range: bytes 0-81/81\r\n\r\n")
        (test-equal? "(head) end early"
                     (output output-file tmp-file 'head #"text/html"
                             0 10)
                     #"HTTP/1.1 206 Okay\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nContent-Length: 10\r\nContent-Range: bytes 0-10/81\r\n\r\n")
        (test-equal? "(head) start late"
                     (output output-file tmp-file 'head #"text/html"
                             10 +inf.0)
                     #"HTTP/1.1 206 Okay\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nContent-Length: 71\r\nContent-Range: bytes 10-81/81\r\n\r\n")
        (test-equal? "(head) start late and end early"
                     (output output-file tmp-file 'head #"text/html"
                             1 10)
                     #"HTTP/1.1 206 Okay\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nContent-Length: 9\r\nContent-Range: bytes 1-10/81\r\n\r\n"))))))