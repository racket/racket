(module response-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (planet "util.ss" ("schematics" "schemeunit.plt" 2))
           (lib "xml.ss" "xml")
           (lib "file.ss")
           (lib "response.ss" "web-server" "private")
           (lib "request-structs.ss" "web-server" "private")
           (lib "response-structs.ss" "web-server" "private")
           "../util.ss")
  
  (require/expose (lib "response.ss" "web-server" "private")
                  (convert-http-ranges
                   make-content-length-header
                   make-content-range-header
                   output-file/boundary))
  
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
       
      (test-equal?
       "convert-http-ranges"
       (convert-http-ranges 
        '((10 . #f) (20 . 30) (#f . 40) (40 . 60) (49 . 60))
        50)
       '((10 . 50) (20 . 31) (10 . 50) (40 . 50)))

       (test-suite
        "output-file"
        
        (test-equal? "(get) whole file - no Range header"
                     (output output-file tmp-file 'get #"text/html" #f)
                     #"HTTP/1.1 200 OK\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nAccept-Ranges: bytes\r\nContent-Length: 81\r\n\r\n<html><head><title>A title</title></head><body>Here's some content!</body></html>")

        (test-equal? "(get) whole file - Range header present"
                     (output output-file tmp-file 'get #"text/html" '((0 . 80)))
                     #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nAccept-Ranges: bytes\r\nContent-Length: 81\r\nContent-Range: bytes 0-80/81\r\n\r\n<html><head><title>A title</title></head><body>Here's some content!</body></html>")

        (test-equal? "(get) single range - suffix range larger than file"
                     (output output-file tmp-file 'get #"text/html" '((#f . 90)))
                     #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nAccept-Ranges: bytes\r\nContent-Length: 81\r\nContent-Range: bytes 0-80/81\r\n\r\n<html><head><title>A title</title></head><body>Here's some content!</body></html>")
        
        (test-equal? "(get) single range - 10 bytes from the start"
                     (output output-file tmp-file 'get #"text/html" '((0 . 9)))
                     #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nAccept-Ranges: bytes\r\nContent-Length: 10\r\nContent-Range: bytes 0-9/81\r\n\r\n<html><hea")
        
        (test-equal? "(get) single range - 10 bytes from the end"
                     (output output-file tmp-file 'get #"text/html" '((71 . #f)))
                     #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nAccept-Ranges: bytes\r\nContent-Length: 10\r\nContent-Range: bytes 71-80/81\r\n\r\ndy></html>")
        
        (test-equal? "(get) single range - 10 bytes from past the end"
                     (output output-file tmp-file 'get #"text/html" '((76 . 86)))
                     #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nAccept-Ranges: bytes\r\nContent-Length: 5\r\nContent-Range: bytes 76-80/81\r\n\r\nhtml>")
        
        (test-equal? "(get) single range - 10 bytes from the middle"
                     (output output-file tmp-file 'get #"text/html" '((10 . 19)))
                     #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nAccept-Ranges: bytes\r\nContent-Length: 10\r\nContent-Range: bytes 10-19/81\r\n\r\nd><title>A")
        
        (test-equal? "(get) multiple ranges"
                     (output output-file/boundary tmp-file 'get #"text/html" '((10 . 19) (30 . 39) (50 . 59)) #"BOUNDARY")
                     #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: multipart/byteranges; boundary=BOUNDARY\r\nAccept-Ranges: bytes\r\nContent-Length: 260\r\n\r\n--BOUNDARY\r\nContent-Type: text/html\r\nContent-Range: bytes 10-19/81\r\n\r\nd><title>A\r\n--BOUNDARY\r\nContent-Type: text/html\r\nContent-Range: bytes 30-39/81\r\n\r\ntle></head\r\n--BOUNDARY\r\nContent-Type: text/html\r\nContent-Range: bytes 50-59/81\r\n\r\ne's some c\r\n--BOUNDARY--\r\n")
        
        (test-equal? "(get) some bad ranges"
                     (output output-file/boundary tmp-file 'get #"text/html" '((10 . 19) (1000 . 1050) (30 . 39) (50 . 49)) #"BOUNDARY")
                     #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: multipart/byteranges; boundary=BOUNDARY\r\nAccept-Ranges: bytes\r\nContent-Length: 178\r\n\r\n--BOUNDARY\r\nContent-Type: text/html\r\nContent-Range: bytes 10-19/81\r\n\r\nd><title>A\r\n--BOUNDARY\r\nContent-Type: text/html\r\nContent-Range: bytes 30-39/81\r\n\r\ntle></head\r\n--BOUNDARY--\r\n")
        
        (test-equal? "(get) all bad ranges"
                     (output output-file/boundary tmp-file 'get #"text/html" '((-10 . -5) (1000 . 1050) (50 . 49)) #"BOUNDARY")
                     #"HTTP/1.1 416 Invalid range request\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\n\r\n")
        
        (test-equal? "(head) whole file - no Range header"
                     (output output-file tmp-file 'head #"text/html" #f)
                     #"HTTP/1.1 200 OK\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nAccept-Ranges: bytes\r\nContent-Length: 81\r\n\r\n")
        
        (test-equal? "(head) whole file - Range header present"
                     (output output-file tmp-file 'head #"text/html" '((0 . 80)))
                     #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nAccept-Ranges: bytes\r\nContent-Length: 81\r\nContent-Range: bytes 0-80/81\r\n\r\n")

        (test-equal? "(head) single range - 10 bytes from the start"
                     (output output-file tmp-file 'head #"text/html" '((0 . 9)))
                     #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nAccept-Ranges: bytes\r\nContent-Length: 10\r\nContent-Range: bytes 0-9/81\r\n\r\n")
        
        (test-equal? "(head) single range - 10 bytes from the end"
                     (output output-file tmp-file 'head #"text/html" '((71 . #f)))
                     #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nAccept-Ranges: bytes\r\nContent-Length: 10\r\nContent-Range: bytes 71-80/81\r\n\r\n")
        
        (test-equal? "(head) single range - 10 bytes from the middle"
                     (output output-file tmp-file 'head #"text/html" '((10 . 19)))
                     #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nAccept-Ranges: bytes\r\nContent-Length: 10\r\nContent-Range: bytes 10-19/81\r\n\r\n")
        
        (test-equal? "(head) multiple ranges"
                     (output output-file/boundary tmp-file 'head #"text/html" '((10 . 19) (30 . 39) (50 . 59)) #"BOUNDARY")
                     #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: multipart/byteranges; boundary=BOUNDARY\r\nAccept-Ranges: bytes\r\nContent-Length: 260\r\n\r\n")
        
        (test-equal? "(head) some bad ranges"
                     (output output-file/boundary tmp-file 'head #"text/html" '((10 . 19) (1000 . 1050) (30 . 39) (50 . 49)) #"BOUNDARY")
                     #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: multipart/byteranges; boundary=BOUNDARY\r\nAccept-Ranges: bytes\r\nContent-Length: 178\r\n\r\n")
        
        (test-equal? "(head) all bad ranges"
                     (output output-file/boundary tmp-file 'head #"text/html" '((-10 . -5) (1000 . 1050) (50 . 49)) #"BOUNDARY")
                     #"HTTP/1.1 416 Invalid range request\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\n\r\n")
        
        ))))
  
  )