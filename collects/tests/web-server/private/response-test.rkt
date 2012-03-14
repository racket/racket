#lang racket/base
(require rackunit
         racket/port
         xml/xml
         (only-in mzlib/file
                  make-temporary-file)
         web-server/http
         web-server/http/response
         (prefix-in compat0: web-server/compat/0/http/response-structs)
         "../util.rkt")

(require/expose web-server/http/response
                (convert-http-ranges
                 make-content-length-header
                 make-content-range-header
                 output-file/boundary))

(provide response-tests)

(define (output f . any)
  (define-values (c i o) (make-mock-connection #""))
  (apply f c any)
  (redact (get-output-bytes o)))

(define-syntax-rule (test-equi? t a e)
  (test-equal? t (bytes-sort a) (bytes-sort e)))

(define output-response-tests
  (test-suite
    "output-response"
    
    (test-suite 
     "response"
     (test-equi? "response" 
                  (output output-response 
                          (response 404 #"404" (current-seconds) #"text/html"
                                    (list) void))
                  #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\n\r\n")
     (test-equi? "response" 
                  (output output-response 
                          (response 404 #"404" (current-seconds) #f
                                    (list) void))
                  #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\n\r\n")
     (test-equi? "response (header)" 
                  (output output-response 
                          (response 404 #"404" (current-seconds) #"text/html"
                                    (list (make-header #"Header" #"Value")) void))
                  #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\nHeader: Value\r\n\r\n")
     (test-equi? "response (body)" 
                  (output output-response 
                          (response 404 #"404" (current-seconds) #"text/html"
                                    (list) void))
                  #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\n\r\n")
     (test-equi? "response (bytes body)"
                  (output output-response 
                          (response 404 #"404" (current-seconds) #"text/html"
                                    (list) void))
                  #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\n\r\n")
     (test-equi? "response (both)" 
                  (output output-response 
                          (response 404 #"404" (current-seconds) #"text/html"
                                    (list (make-header #"Header" #"Value")) void))
                  #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\nHeader: Value\r\n\r\n")
     (test-equi? "response (both)" 
                  (output output-response 
                          (response 404 #"404" (current-seconds) #"text/html"
                                    (list (make-header #"Header" #"Value1")
                                          (make-header #"Header" #"Value2")) void))
                  #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\nHeader: Value1\r\nHeader: Value2\r\n\r\n"))
    
    (test-suite 
     "response/full"
     (test-equi? "response/full" 
                  (output output-response 
                          (response/full 404 #"404" (current-seconds) #"text/html"
                                              (list) (list)))
                  #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\nContent-Length: 0\r\n\r\n")
     (test-equi? "response/full (header)" 
                  (output output-response 
                          (response/full 404 #"404" (current-seconds) #"text/html"
                                              (list (make-header #"Header" #"Value")) (list)))
                  #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\nContent-Length: 0\r\nHeader: Value\r\n\r\n")
     
     (test-equi? "response/full (bytes body)"
                  (output output-response 
                          (response/full 404 #"404" (current-seconds) #"text/html"
                                              (list) (list #"Content!")))
                  #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\nContent-Length: 8\r\n\r\nContent!")
     (test-equi? "response/full (both)" 
                  (output output-response 
                          (response/full 404 #"404" (current-seconds) #"text/html"
                                              (list (make-header #"Header" #"Value")) (list #"Content!")))
                  #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\nContent-Length: 8\r\nHeader: Value\r\n\r\nContent!"))
    
    (test-suite
     "Simple content"
     (test-equi? "empty"
                  (output output-response
                          (compat0:normalize-response (list #"text/html")))
                  #"HTTP/1.1 200 Okay\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\nContent-Length: 0\r\n\r\n")
     (test-equi? "not"
                  (output output-response
                          (compat0:normalize-response (list #"text/html" "Content")))
                  #"HTTP/1.1 200 Okay\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\nContent-Length: 7\r\n\r\nContent")
     (test-equi? "not, bytes"
                  (output output-response
                          (compat0:normalize-response (list #"text/html" #"Content")))
                  #"HTTP/1.1 200 Okay\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\nContent-Length: 7\r\n\r\nContent"))
    
    (test-suite
     "xexpr"
     (test-equi? "any"
                  (output output-response
                          (response/xexpr `(html (head (title "Hey!")) (body "Content"))))
                  #"HTTP/1.1 200 Okay\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html; charset=utf-8\r\n\r\n<html><head><title>Hey!</title></head><body>Content</body></html>"))
    ))

(define output-response/method-tests
  (test-suite
    "output-response/method"
    
    (test-suite 
     "response/full"
     (test-equi? "response/full" 
                  (output output-response/method 
                          (response/full 404 #"404" (current-seconds) #"text/html"
                                              (list) (list))
                          #"HEAD")
                  #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\nContent-Length: 0\r\n\r\n")
     (test-equi? "response/full (header)" 
                  (output output-response/method
                          (response/full 404 #"404" (current-seconds) #"text/html"
                                              (list (make-header #"Header" #"Value")) (list))
                          #"HEAD")
                  #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\nContent-Length: 0\r\nHeader: Value\r\n\r\n")
     (test-equi? "response/full (body)" 
                  (output output-response/method
                          (response/full 404 #"404" (current-seconds) #"text/html"
                                              (list) (list #"Content!"))
                          #"HEAD")
                  #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\nContent-Length: 8\r\n\r\n")
     (test-equi? "response/full (bytes body)"
                  (output output-response/method 
                          (response/full 404 #"404" (current-seconds) #"text/html"
                                              (list) (list #"Content!"))
                          #"HEAD")
                  #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\nContent-Length: 8\r\n\r\n")
     (test-equi? "response/full (both)" 
                  (output output-response/method
                          (response/full 404 #"404" (current-seconds) #"text/html"
                                              (list (make-header #"Header" #"Value")) (list #"Content!"))
                          #"HEAD")
                  #"HTTP/1.1 404 404\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\nContent-Length: 8\r\nHeader: Value\r\n\r\n"))
    
    (test-suite
     "Simple content"
     (test-equi? "empty"
                  (output output-response/method
                          (compat0:normalize-response (list #"text/html"))
                          #"HEAD")
                  #"HTTP/1.1 200 Okay\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\nContent-Length: 0\r\n\r\n")
     (test-equi? "not"
                  (output output-response/method
                          (compat0:normalize-response (list #"text/html" #"Content"))
                          #"HEAD")
                  #"HTTP/1.1 200 Okay\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\nContent-Length: 7\r\n\r\n")
     (test-equi? "not, bytes"
                  (output output-response/method
                          (compat0:normalize-response (list #"text/html" #"Content"))
                          #"HEAD")
                  #"HTTP/1.1 200 Okay\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\nContent-Length: 7\r\n\r\n"))
    
    (test-suite
     "xexpr"
     (test-equi? "any"
                  (output output-response/method
                          (response/xexpr `(html (head (title "Hey!")) (body "Content")))
                          #"HEAD")
                  #"HTTP/1.1 200 Okay\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html; charset=utf-8\r\n\r\n"))))

(define response-tests
  (test-suite
   "HTTP Responses"
   
   output-response-tests
   
   output-response/method-tests
   
   (let ()
     (define tmp-file (make-temporary-file))
     (with-output-to-file tmp-file 
       (lambda ()
         (display
          (xexpr->string 
           `(html (head (title "A title"))
                  (body "Here's some content!")))))
       #:exists 'truncate/replace)
     
     (test-equal?
      "convert-http-ranges"
      (convert-http-ranges 
       '((10 . #f) (20 . 30) (#f . 40) (40 . 60) (49 . 60))
       50)
      '((10 . 50) (20 . 31) (10 . 50) (40 . 50) (49 . 50)))
     
     (test-suite
      "output-file"
      
      (test-equi? "(get) whole file - no Range header"
                   (output output-file tmp-file #"GET" #"text/html" #f)
                   #"HTTP/1.1 200 OK\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\nAccept-Ranges: bytes\r\nContent-Length: 81\r\n\r\n<html><head><title>A title</title></head><body>Here's some content!</body></html>")
      
      (test-equi? "(get) whole file - Range header present"
                   (output output-file tmp-file #"GET" #"text/html" '((0 . 80)))
                   #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\nAccept-Ranges: bytes\r\nContent-Length: 81\r\nContent-Range: bytes 0-80/81\r\n\r\n<html><head><title>A title</title></head><body>Here's some content!</body></html>")
      
      (test-equi? "(get) single range - suffix range larger than file"
                   (output output-file tmp-file #"GET" #"text/html" '((#f . 90)))
                   #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\nAccept-Ranges: bytes\r\nContent-Length: 81\r\nContent-Range: bytes 0-80/81\r\n\r\n<html><head><title>A title</title></head><body>Here's some content!</body></html>")
      
      (test-equi? "(get) single range - 10 bytes from the start"
                   (output output-file tmp-file #"GET" #"text/html" '((0 . 9)))
                   #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\nAccept-Ranges: bytes\r\nContent-Length: 10\r\nContent-Range: bytes 0-9/81\r\n\r\n<html><hea")
      
      (test-equi? "(get) single range - 10 bytes from the end"
                   (output output-file tmp-file #"GET" #"text/html" '((71 . #f)))
                   #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\nAccept-Ranges: bytes\r\nContent-Length: 10\r\nContent-Range: bytes 71-80/81\r\n\r\ndy></html>")
      
      (test-equi? "(get) single range - 10 bytes from past the end"
                   (output output-file tmp-file #"GET" #"text/html" '((76 . 86)))
                   #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\nAccept-Ranges: bytes\r\nContent-Length: 5\r\nContent-Range: bytes 76-80/81\r\n\r\nhtml>")
      
      (test-equi? "(get) single range - 10 bytes from the middle"
                   (output output-file tmp-file #"GET" #"text/html" '((10 . 19)))
                   #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\nAccept-Ranges: bytes\r\nContent-Length: 10\r\nContent-Range: bytes 10-19/81\r\n\r\nd><title>A")
      
      (test-equi? "(get) multiple ranges"
                   (output output-file/boundary tmp-file #"GET" #"text/html" '((10 . 19) (30 . 39) (50 . 59)) #"BOUNDARY")
                   #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: multipart/byteranges; boundary=BOUNDARY\r\nAccept-Ranges: bytes\r\nContent-Length: 266\r\n\r\n--BOUNDARY\r\nContent-Type: text/html\r\nContent-Range: bytes 10-19/81\r\n\r\nd><title>A\r\n\r\n--BOUNDARY\r\nContent-Type: text/html\r\nContent-Range: bytes 30-39/81\r\n\r\ntle></head\r\n\r\n--BOUNDARY\r\nContent-Type: text/html\r\nContent-Range: bytes 50-59/81\r\n\r\ne's some c\r\n\r\n--BOUNDARY--\r\n")
      
      (test-equi? "(get) some bad ranges"
                  (parameterize ([current-error-port (open-output-nowhere)])
                   (output output-file/boundary tmp-file #"GET" #"text/html" '((10 . 19) (1000 . 1050) (30 . 39) (50 . 49)) #"BOUNDARY"))
                   #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: multipart/byteranges; boundary=BOUNDARY\r\nAccept-Ranges: bytes\r\nContent-Length: 182\r\n\r\n--BOUNDARY\r\nContent-Type: text/html\r\nContent-Range: bytes 10-19/81\r\n\r\nd><title>A\r\n\r\n--BOUNDARY\r\nContent-Type: text/html\r\nContent-Range: bytes 30-39/81\r\n\r\ntle></head\r\n\r\n--BOUNDARY--\r\n")
      
      (test-equi? "(get) all bad ranges"
                  (parameterize ([current-error-port (open-output-nowhere)])
                   (output output-file/boundary tmp-file #"GET" #"text/html" '((-10 . -5) (1000 . 1050) (50 . 49)) #"BOUNDARY"))
                   #"HTTP/1.1 416 Invalid range request\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\n\r\n")
      
      (test-equal? "(get) some bad ranges (error)"
                   (let ()
                     (define os (open-output-string))
                     (parameterize ([current-error-port os])
                       (output output-file/boundary tmp-file #"GET" #"text/html" '((10 . 19) (1000 . 1050) (30 . 39) (50 . 49)) #"BOUNDARY"))
                     (get-output-string os))
                   "")
      
      (test-equal? "(get) all bad ranges (error)"
                   (let ()
                     (define os (open-output-string))
                     (parameterize ([current-error-port os])
                   (output output-file/boundary tmp-file #"GET" #"text/html" '((-10 . -5) (1000 . 1050) (50 . 49)) #"BOUNDARY"))
                     (get-output-string os))
                   "")
      
      (test-equi? "(head) whole file - no Range header"
                   (output output-file tmp-file #"HEAD" #"text/html" #f)
                   #"HTTP/1.1 200 OK\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\nAccept-Ranges: bytes\r\nContent-Length: 81\r\n\r\n")
      
      (test-equi? "(head) whole file - Range header present"
                   (output output-file tmp-file #"HEAD" #"text/html" '((0 . 80)))
                   #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\nAccept-Ranges: bytes\r\nContent-Length: 81\r\nContent-Range: bytes 0-80/81\r\n\r\n")
      
      (test-equi? "(head) single range - 10 bytes from the start"
                   (output output-file tmp-file #"HEAD" #"text/html" '((0 . 9)))
                   #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\nAccept-Ranges: bytes\r\nContent-Length: 10\r\nContent-Range: bytes 0-9/81\r\n\r\n")
      
      (test-equi? "(head) single range - 10 bytes from the end"
                   (output output-file tmp-file #"HEAD" #"text/html" '((71 . #f)))
                   #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\nAccept-Ranges: bytes\r\nContent-Length: 10\r\nContent-Range: bytes 71-80/81\r\n\r\n")
      
      (test-equi? "(head) single range - 10 bytes from the middle"
                   (output output-file tmp-file #"HEAD" #"text/html" '((10 . 19)))
                   #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\nAccept-Ranges: bytes\r\nContent-Length: 10\r\nContent-Range: bytes 10-19/81\r\n\r\n")
      
      (test-equi? "(head) multiple ranges"
                   (output output-file/boundary tmp-file #"HEAD" #"text/html" '((10 . 19) (30 . 39) (50 . 59)) #"BOUNDARY")
                   #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: multipart/byteranges; boundary=BOUNDARY\r\nAccept-Ranges: bytes\r\nContent-Length: 266\r\n\r\n")
      
      (test-equi? "(head) some bad ranges"
                   (output output-file/boundary tmp-file #"HEAD" #"text/html" '((10 . 19) (1000 . 1050) (30 . 39) (50 . 49)) #"BOUNDARY")
                   #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: multipart/byteranges; boundary=BOUNDARY\r\nAccept-Ranges: bytes\r\nContent-Length: 182\r\n\r\n")
      
      (test-equi? "(head) all bad ranges"
                  (parameterize ([current-error-port (open-output-nowhere)])
                   (output output-file/boundary tmp-file #"HEAD" #"text/html" '((-10 . -5) (1000 . 1050) (50 . 49)) #"BOUNDARY"))
                   #"HTTP/1.1 416 Invalid range request\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\n\r\n")
      
      (test-equal? "(head) all bad ranges"
                  (let ()
                     (define os (open-output-string))
                     (parameterize ([current-error-port os])
                   
                   (output output-file/boundary tmp-file #"HEAD" #"text/html" '((-10 . -5) (1000 . 1050) (50 . 49)) #"BOUNDARY"))
                     (get-output-string os))
                  "")))))

(module+ test
  (require rackunit/text-ui)
  (run-tests response-tests))
