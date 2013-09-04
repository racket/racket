#lang racket/base
(module+ test
  (require rackunit
           racket/tcp
           racket/port
           racket/list
           (prefix-in hc: net/http-client)
           (prefix-in u: net/url))

  (define (port->bytes* in)
    (define ob (open-output-bytes))
    (let loop ()
      (sleep)
      (when (byte-ready? in)
        (define b (read-byte in))
        (unless (eof-object? b)
          (write-byte b ob)
          (loop))))
    (get-output-bytes ob))

  (define-syntax-rule (tests [t ...] ...)
    (begin (test t ...) ...))

  (define-syntax-rule (test-e the-port e raw ereq estatus eheaders econtent)
    (let ()
      (define l (tcp-listen 0 40 #t "127.0.0.1"))
      (define-values (_1 the-port _2 _3)
        (tcp-addresses l #t))
      (define req #f)
      (define lt
        (thread
         (λ ()
           (define-values (in out) (tcp-accept l))
           (tcp-close l)
           (display raw out)
           (flush-output out)
           (tcp-abandon-port out)
           (close-output-port out)
           (set! req (port->bytes* in))
           (tcp-abandon-port in)
           (close-input-port in))))
      (define-values (status headers content-port)
        e)
      (thread-wait lt)
      (check-equal? req ereq)
      (check-equal? status estatus)
      (check-equal? headers eheaders)
      (check-equal? (port->bytes content-port) econtent)))

  (define-syntax-rule (test raw ereq estatus eheaders econtent)
    (begin
      (test-e the-port
              (hc:http-sendrecv "localhost" "/"
                                #:ssl? #f
                                #:port the-port
                                #:method "GET"
                                #:headers empty
                                #:data #f)
              raw ereq estatus eheaders econtent)
      (test-e the-port
              (u:http-sendrecv/url
               (u:make-url "http" #f "localhost" the-port #t (list (u:path/param "" empty)) empty #f)
               #:method "GET"
               #:headers empty
               #:data #f)
              raw ereq estatus eheaders econtent)))

  (tests
   ["HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n24\r\nThis is the data in the first chunk \r\n1A\r\nand this is the second one\r\n0\r\n"
    #"GET / HTTP/1.1\r\nHost: localhost\r\nAccept-Encoding: gzip\r\n\r\n"
    #"HTTP/1.1 200 OK"
    '(#"Content-Type: text/plain" #"Transfer-Encoding: chunked")
    #"This is the data in the first chunk and this is the second one"]

   ["HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\n\r\nThis is the data in the first chunk and this is the second one"
    #"GET / HTTP/1.1\r\nHost: localhost\r\nAccept-Encoding: gzip\r\n\r\n"
    #"HTTP/1.0 200 OK"
    '(#"Content-Type: text/plain")
    #"This is the data in the first chunk and this is the second one"]

   ["HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 62\r\n\r\nThis is the data in the first chunk and this is the second one"
    #"GET / HTTP/1.1\r\nHost: localhost\r\nAccept-Encoding: gzip\r\n\r\n"
    #"HTTP/1.0 200 OK"
    '(#"Content-Type: text/plain" #"Content-Length: 62")
    #"This is the data in the first chunk and this is the second one"]

   ["HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\ncontent-length: 62\r\n\r\nThis is the data in the first chunk and this is the second one"
    #"GET / HTTP/1.1\r\nHost: localhost\r\nAccept-Encoding: gzip\r\n\r\n"
    #"HTTP/1.0 200 OK"
    '(#"Content-Type: text/plain" #"content-length: 62")
    #"This is the data in the first chunk and this is the second one"]

   ["HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n20\r\nThis is the data in the first ch\r\n21\r\nand this is the second oneXXXXXXX\r\n0\r\n"

    #"GET / HTTP/1.1\r\nHost: localhost\r\nAccept-Encoding: gzip\r\n\r\n"
    #"HTTP/1.1 200 OK"
    '(#"Content-Type: text/plain" #"Transfer-Encoding: chunked")
    #"This is the data in the first chand this is the second oneXXXXXXX"]

   ["HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n24\r\nThis is the data in the first chunk \r\n1A\r\nand this is the second one\r\n0\r\n"
    #"GET / HTTP/1.1\r\nHost: localhost\r\nAccept-Encoding: gzip\r\n\r\n"
    #"HTTP/1.1 200 OK"
    '(#"Content-Type: text/plain" #"Transfer-Encoding: chunked")
    #"This is the data in the first chunk and this is the second one"]

   ["HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\n\r\nThis is the data in the first chunk and this is the second one"
    #"GET / HTTP/1.1\r\nHost: localhost\r\nAccept-Encoding: gzip\r\n\r\n"
    #"HTTP/1.0 200 OK"
    '(#"Content-Type: text/plain")
    #"This is the data in the first chunk and this is the second one"]

   ["HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n20\r\nThis is the data in the first ch\r\n21\r\nand this is the second oneXXXXXXX\r\n0\r\n"
    #"GET / HTTP/1.1\r\nHost: localhost\r\nAccept-Encoding: gzip\r\n\r\n"
    #"HTTP/1.1 200 OK"
    '(#"Content-Type: text/plain" #"Transfer-Encoding: chunked")
    #"This is the data in the first chand this is the second oneXXXXXXX"]

   ["HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\n\r\nThis is the data in the first chunk and this is the second one\r\n"
    #"GET / HTTP/1.1\r\nHost: localhost\r\nAccept-Encoding: gzip\r\n\r\n"
    #"HTTP/1.0 200 OK"
    '(#"Content-Type: text/plain")
    #"This is the data in the first chunk and this is the second one\r\n"]

   ["HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n24\r\nThis is the data in the first chunk \r\n1A\r\nand this is the second one\r\n0\r\n"
    #"GET / HTTP/1.1\r\nHost: localhost\r\nAccept-Encoding: gzip\r\n\r\n"
    #"HTTP/1.1 200 OK"
    '(#"Content-Type: text/plain" #"Transfer-Encoding: chunked")
    #"This is the data in the first chunk and this is the second one"]

   ["HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\n\r\nThis is the data in the first chunk and this is the second one"
    #"GET / HTTP/1.1\r\nHost: localhost\r\nAccept-Encoding: gzip\r\n\r\n"
    #"HTTP/1.0 200 OK"
    '(#"Content-Type: text/plain")
    #"This is the data in the first chunk and this is the second one"]

   ["HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n20\r\nThis is the data in the first ch\r\n21\r\nand this is the second oneXXXXXXX\r\n0\r\n"
    #"GET / HTTP/1.1\r\nHost: localhost\r\nAccept-Encoding: gzip\r\n\r\n"
    #"HTTP/1.1 200 OK"
    '(#"Content-Type: text/plain" #"Transfer-Encoding: chunked")
    #"This is the data in the first chand this is the second oneXXXXXXX"]

   ["HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\nAnother-Header: ta-daa\r\n\r\n20\r\nThis is the data in the first ch\r\n21\r\nand this is the second oneXXXXXXX\r\n0\r\n"
    #"GET / HTTP/1.1\r\nHost: localhost\r\nAccept-Encoding: gzip\r\n\r\n"
    #"HTTP/1.1 200 OK"
    '(#"Content-Type: text/plain" #"Transfer-Encoding: chunked" #"Another-Header: ta-daa")
    #"This is the data in the first chand this is the second oneXXXXXXX"]

   ["HTTP/1.1 301 Moved Permanently\r\nLocation: http://localhost:9002/whatever\r\n\r\nstuff"
    #"GET / HTTP/1.1\r\nHost: localhost\r\nAccept-Encoding: gzip\r\n\r\n"
    #"HTTP/1.1 301 Moved Permanently"
    '(#"Location: http://localhost:9002/whatever")
    #"stuff"]

   ["HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\nAnother-Header: ta-daa\r\n\r\n20\r\nThis is the data in the first ch\r\n21\r\nand this is the second oneXXXXXXX\r\n0\r\n"
    #"GET / HTTP/1.1\r\nHost: localhost\r\nAccept-Encoding: gzip\r\n\r\n"
    #"HTTP/1.1 200 OK"
    '(#"Content-Type: text/plain" #"Transfer-Encoding: chunked" #"Another-Header: ta-daa")
    #"This is the data in the first chand this is the second oneXXXXXXX"]))
