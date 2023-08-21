#lang racket/base
(module+ test
  (require rackunit
           racket/tcp
           racket/port
           (for-syntax racket/base)
           racket/list
           (prefix-in hc: net/http-client)
           (prefix-in u: net/url))

  (define-syntax regexp-replace**
    (syntax-rules ()
      [(_ () s)
       s]
      [(_ ([from to] . more) s)
       (regexp-replace* from (regexp-replace** more s) to)]))

  (define (read-request in)
    ;; Simulate HTTP request reading enough to get the right number
    ;; of bytes.
    (define ob (open-output-bytes))
    (regexp-match #rx"\r\n\r\n" in 0 #f ob)
    (write-bytes #"\r\n\r\n" ob)
    (define bstr (get-output-bytes ob))
    (cond
     [(regexp-match #rx"Content-Length: ([0-9]+)" bstr)
      => (lambda (m)
           (bytes-append bstr (read-bytes
                               (string->number (bytes->string/utf-8 (cadr m)))
                               in)))]
     [(regexp-match? #rx"^PUT" bstr)
      (bytes-append bstr (read-request in))]
     [else bstr]))

  (define-syntax (tests stx)
    (syntax-case stx ()
      [(_ t ...)
       (with-syntax
           ([(t.test ...)
             (for/list ([t (in-list (syntax->list #'(t ...)))])
               (quasisyntax/loc t
                 (test . #,t)))])
         (syntax/loc stx
           (begin t.test ...)))]))

  (define-syntax (test-e stx)
    (syntax-case stx ()
      [(_ the-port repeats init-e proc-e raw ereq estatus eheaders econtent)
       (quasisyntax/loc stx
         (let ([v init-e])
           (for ([i repeats])
             (define l (tcp-listen 0 40 #t "127.0.0.1"))
             (define-values (_1 the-port _2 _3)
               (tcp-addresses l #t))
             (define req #f)
             (define lt
               (thread
                (λ ()
                  (define-values (in out) (tcp-accept l))
                  (tcp-close l)
                  (set! req (read-request in))
                  (display raw out)
                  (flush-output out)
                  (close-output-port out)
                  (close-input-port in))))
             (define-values (status headers content-port)
               (proc-e v))
             #,(syntax/loc stx
                 (check-equal? status estatus))
             #,(syntax/loc stx
                 (check-equal? headers eheaders))
             #,(syntax/loc stx
                 (check-equal? (port->bytes content-port) econtent))
             (thread-wait lt)
             #,(syntax/loc stx
                 (check-equal?
                  (regexp-replace** ([(number->string the-port) "REDACTED"]
                                     [(regexp-quote (version)) "REDACTED"])
                                    req)
                  ereq)))))]))

  (define-syntax (test stx)
    (syntax-case stx ()
      [(_ method body raw ereq estatus eheaders econtent extra ...)
       (quasisyntax/loc stx
         (begin
           #,(syntax/loc stx
               (test-e the-port
                       1 #f
                       (lambda (ignored)
                         (hc:http-sendrecv "localhost" "/"
                                           #:ssl? #f
                                           #:port the-port
                                           #:method method
                                           #:headers empty
                                           #:data body
                                           extra ...))
                       raw ereq estatus eheaders econtent))
           #,(syntax/loc stx
               (test-e the-port
                       1 #f
                       (lambda (ignored)
                         (let ([c (hc:http-conn-open "localhost"
                                                     #:port the-port
                                                     #:ssl? #f)])
                           (check-equal? #t (hc:http-conn-live? c))
                           (check-equal? #t (hc:http-conn-liveable? c))
                           (hc:http-conn-send! c
                                               "/"
                                               #:method method
                                               #:headers empty
                                               #:close? #t
                                               #:data body
                                               extra ...)
                           (begin0
                            (hc:http-conn-recv! c
                                                #:method method
                                                #:close? #t)
                            (check-equal? #f (hc:http-conn-live? c))
                            (check-equal? #f (hc:http-conn-liveable? c)))))
                       raw ereq estatus eheaders econtent))
           #,(syntax/loc stx
               (test-e the-port
                       3 (hc:http-conn)
                       (lambda (c)
                         (check-equal? #f (hc:http-conn-live? c))
                         (check-equal? #f (hc:http-conn-liveable? c))
                         (hc:http-conn-open! c "localhost"
                                             #:port the-port
                                             #:ssl? #f)
                         (check-equal? #t (hc:http-conn-live? c))
                         (check-equal? #t (hc:http-conn-liveable? c))
                         (hc:http-conn-send! c
                                             "/"
                                             #:method method
                                             #:headers empty
                                             #:close? #t
                                             #:data body
                                             extra ...)
                         (begin0
                          (hc:http-conn-recv! c
                                              #:method method
                                              #:close? #t)
                          (check-equal? #f (hc:http-conn-live? c))
                          (check-equal? #f (hc:http-conn-liveable? c))))
                       raw ereq estatus eheaders econtent))
           #,(syntax/loc stx
               (test-e the-port
                       1 #f
                       (lambda (ignored)
                         (u:http-sendrecv/url
                          (u:make-url "http" #f "localhost" the-port #t
                                      (list (u:path/param "" empty)) empty #f)
                          #:method method
                          #:headers empty
                          #:data body
                          extra ...))
                       raw ereq estatus eheaders econtent))))]))

  (tests
   ["GET" #f "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n24\r\nThis is the data in the first chunk \r\n1A\r\nand this is the second one\r\n0\r\n"
    #"GET / HTTP/1.1\r\nHost: localhost:REDACTED\r\nUser-Agent: Racket/REDACTED (net/http-client)\r\nAccept-Encoding: gzip,deflate\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
    #"HTTP/1.1 200 OK"
    '(#"Content-Type: text/plain" #"Transfer-Encoding: chunked")
    #"This is the data in the first chunk and this is the second one"]

   ["GET" #f "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n24\r\nThis is the data in the first chunk \r\n1A\r\nand this is the second one\r\n0\r\n"
    #"GET / HTTP/1.1\r\nHost: localhost:REDACTED\r\nUser-Agent: Racket/REDACTED (net/http-client)\r\nAccept-Encoding: gzip\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
    #"HTTP/1.1 200 OK"
    '(#"Content-Type: text/plain" #"Transfer-Encoding: chunked")
    #"This is the data in the first chunk and this is the second one"
    #:content-decode '(gzip)]

   ["GET" #f "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n24\r\nThis is the data in the first chunk \r\n1A\r\nand this is the second one\r\n0\r\n"
    #"GET / HTTP/1.1\r\nHost: localhost:REDACTED\r\nUser-Agent: Racket/REDACTED (net/http-client)\r\nAccept-Encoding: deflate\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
    #"HTTP/1.1 200 OK"
    '(#"Content-Type: text/plain" #"Transfer-Encoding: chunked")
    #"This is the data in the first chunk and this is the second one"
    #:content-decode '(deflate)]

   ["GET" #f "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n24\r\nThis is the data in the first chunk \r\n1A\r\nand this is the second one\r\n0\r\n"
    #"GET / HTTP/1.1\r\nHost: localhost:REDACTED\r\nUser-Agent: Racket/REDACTED (net/http-client)\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
    #"HTTP/1.1 200 OK"
    '(#"Content-Type: text/plain" #"Transfer-Encoding: chunked")
    #"This is the data in the first chunk and this is the second one"
    #:content-decode '()]

   ["GET" #f "HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\n\r\nThis is the data in the first chunk and this is the second one"
    #"GET / HTTP/1.1\r\nHost: localhost:REDACTED\r\nUser-Agent: Racket/REDACTED (net/http-client)\r\nAccept-Encoding: gzip,deflate\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
    #"HTTP/1.0 200 OK"
    '(#"Content-Type: text/plain")
    #"This is the data in the first chunk and this is the second one"]

   ["GET" #f "HTTP/1.0 200 OK\nContent-Type: text/plain\n\nThis is the data in the first chunk and this is the second one"
    #"GET / HTTP/1.1\r\nHost: localhost:REDACTED\r\nUser-Agent: Racket/REDACTED (net/http-client)\r\nAccept-Encoding: gzip,deflate\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
    #"HTTP/1.0 200 OK"
    '(#"Content-Type: text/plain")
    #"This is the data in the first chunk and this is the second one"]

   ["GET" #f "HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 62\r\n\r\nThis is the data in the first chunk and this is the second one"
    #"GET / HTTP/1.1\r\nHost: localhost:REDACTED\r\nUser-Agent: Racket/REDACTED (net/http-client)\r\nAccept-Encoding: gzip,deflate\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
    #"HTTP/1.0 200 OK"
    '(#"Content-Type: text/plain" #"Content-Length: 62")
    #"This is the data in the first chunk and this is the second one"]

   ["GET" #f "HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\n\r\nThis is the data in the first chunk and this is the second one"
    #"GET / HTTP/1.1\r\nHost: localhost:REDACTED\r\nUser-Agent: Racket/REDACTED (net/http-client)\r\nAccept-Encoding: gzip,deflate\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
    #"HTTP/1.0 200 OK"
    '(#"Content-Type: text/plain")
    #"This is the data in the first chunk and this is the second one"]

   ["GET" #f "HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\ncontent-length: 62\r\n\r\nThis is the data in the first chunk and this is the second one"
    #"GET / HTTP/1.1\r\nHost: localhost:REDACTED\r\nUser-Agent: Racket/REDACTED (net/http-client)\r\nAccept-Encoding: gzip,deflate\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
    #"HTTP/1.0 200 OK"
    '(#"Content-Type: text/plain" #"content-length: 62")
    #"This is the data in the first chunk and this is the second one"]

   ["GET" #f "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n20\r\nThis is the data in the first ch\r\n21\r\nand this is the second oneXXXXXXX\r\n0\r\n"
    #"GET / HTTP/1.1\r\nHost: localhost:REDACTED\r\nUser-Agent: Racket/REDACTED (net/http-client)\r\nAccept-Encoding: gzip,deflate\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
    #"HTTP/1.1 200 OK"
    '(#"Content-Type: text/plain" #"Transfer-Encoding: chunked")
    #"This is the data in the first chand this is the second oneXXXXXXX"]

   ["GET" #f "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n24\r\nThis is the data in the first chunk \r\n1A\r\nand this is the second one\r\n0\r\n"
    #"GET / HTTP/1.1\r\nHost: localhost:REDACTED\r\nUser-Agent: Racket/REDACTED (net/http-client)\r\nAccept-Encoding: gzip,deflate\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
    #"HTTP/1.1 200 OK"
    '(#"Content-Type: text/plain" #"Transfer-Encoding: chunked")
    #"This is the data in the first chunk and this is the second one"]

   ["GET" #f "HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\n\r\nThis is the data in the first chunk and this is the second one"
    #"GET / HTTP/1.1\r\nHost: localhost:REDACTED\r\nUser-Agent: Racket/REDACTED (net/http-client)\r\nAccept-Encoding: gzip,deflate\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
    #"HTTP/1.0 200 OK"
    '(#"Content-Type: text/plain")
    #"This is the data in the first chunk and this is the second one"]

   ["GET" #f "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n20\r\nThis is the data in the first ch\r\n21\r\nand this is the second oneXXXXXXX\r\n0\r\n"
    #"GET / HTTP/1.1\r\nHost: localhost:REDACTED\r\nUser-Agent: Racket/REDACTED (net/http-client)\r\nAccept-Encoding: gzip,deflate\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
    #"HTTP/1.1 200 OK"
    '(#"Content-Type: text/plain" #"Transfer-Encoding: chunked")
    #"This is the data in the first chand this is the second oneXXXXXXX"]

   ["GET" #f "HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\n\r\nThis is the data in the first chunk and this is the second one\r\n"
    #"GET / HTTP/1.1\r\nHost: localhost:REDACTED\r\nUser-Agent: Racket/REDACTED (net/http-client)\r\nAccept-Encoding: gzip,deflate\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
    #"HTTP/1.0 200 OK"
    '(#"Content-Type: text/plain")
    #"This is the data in the first chunk and this is the second one\r\n"]

   ["GET" #f "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n24\r\nThis is the data in the first chunk \r\n1A\r\nand this is the second one\r\n0\r\n"
    #"GET / HTTP/1.1\r\nHost: localhost:REDACTED\r\nUser-Agent: Racket/REDACTED (net/http-client)\r\nAccept-Encoding: gzip,deflate\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
    #"HTTP/1.1 200 OK"
    '(#"Content-Type: text/plain" #"Transfer-Encoding: chunked")
    #"This is the data in the first chunk and this is the second one"]

   ["GET" #f "HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\n\r\nThis is the data in the first chunk and this is the second one"
    #"GET / HTTP/1.1\r\nHost: localhost:REDACTED\r\nUser-Agent: Racket/REDACTED (net/http-client)\r\nAccept-Encoding: gzip,deflate\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
    #"HTTP/1.0 200 OK"
    '(#"Content-Type: text/plain")
    #"This is the data in the first chunk and this is the second one"]

   ["GET" #f "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n20\r\nThis is the data in the first ch\r\n21\r\nand this is the second oneXXXXXXX\r\n0\r\n"
    #"GET / HTTP/1.1\r\nHost: localhost:REDACTED\r\nUser-Agent: Racket/REDACTED (net/http-client)\r\nAccept-Encoding: gzip,deflate\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
    #"HTTP/1.1 200 OK"
    '(#"Content-Type: text/plain" #"Transfer-Encoding: chunked")
    #"This is the data in the first chand this is the second oneXXXXXXX"]

   ["GET" #f "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\nAnother-Header: ta-daa\r\n\r\n20\r\nThis is the data in the first ch\r\n21\r\nand this is the second oneXXXXXXX\r\n0\r\n"
    #"GET / HTTP/1.1\r\nHost: localhost:REDACTED\r\nUser-Agent: Racket/REDACTED (net/http-client)\r\nAccept-Encoding: gzip,deflate\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
    #"HTTP/1.1 200 OK"
    '(#"Content-Type: text/plain" #"Transfer-Encoding: chunked" #"Another-Header: ta-daa")
    #"This is the data in the first chand this is the second oneXXXXXXX"]

   ["GET" #f "HTTP/1.1 301 Moved Permanently\r\nLocation: http://localhost:9002/whatever\r\n\r\nstuff"
    #"GET / HTTP/1.1\r\nHost: localhost:REDACTED\r\nUser-Agent: Racket/REDACTED (net/http-client)\r\nAccept-Encoding: gzip,deflate\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
    #"HTTP/1.1 301 Moved Permanently"
    '(#"Location: http://localhost:9002/whatever")
    #"stuff"]

   ["GET" #f "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\nAnother-Header: ta-daa\r\n\r\n20\r\nThis is the data in the first ch\r\n21\r\nand this is the second oneXXXXXXX\r\n0\r\n"
    #"GET / HTTP/1.1\r\nHost: localhost:REDACTED\r\nUser-Agent: Racket/REDACTED (net/http-client)\r\nAccept-Encoding: gzip,deflate\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
    #"HTTP/1.1 200 OK"
    '(#"Content-Type: text/plain" #"Transfer-Encoding: chunked" #"Another-Header: ta-daa")
    #"This is the data in the first chand this is the second oneXXXXXXX"]

   ["GET" #f "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\nAnother-Header: ta-daa\r\n\r\nbb \r\n<HTML>\n\t\t\t\t\t  <HEAD>\n\t\t\t\t\t  <TITLE>ABCNANOTECH Co., LTD.</TITLE>\n\t\t\t\t\t  </HEAD>\n\t\t\t\t\t  <FRAMESET ROWS=\"100%,*\" border=0>\n\t\t\t\t\t  <FRAME src=http://nanotech.co.kr></FRAMESET>\n\t\t\t\t\t  </HTML>\r\n0\r\n\r\n"
    #"GET / HTTP/1.1\r\nHost: localhost:REDACTED\r\nUser-Agent: Racket/REDACTED (net/http-client)\r\nAccept-Encoding: gzip,deflate\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
    #"HTTP/1.1 200 OK"
    '(#"Content-Type: text/plain" #"Transfer-Encoding: chunked" #"Another-Header: ta-daa")
    #"<HTML>\n\t\t\t\t\t  <HEAD>\n\t\t\t\t\t  <TITLE>ABCNANOTECH Co., LTD.</TITLE>\n\t\t\t\t\t  </HEAD>\n\t\t\t\t\t  <FRAMESET ROWS=\"100%,*\" border=0>\n\t\t\t\t\t  <FRAME src=http://nanotech.co.kr></FRAMESET>\n\t\t\t\t\t  </HTML>"]

   ["PUT" #f "HTTP/1.1 200 OK\r\n\r\n"
    #"PUT / HTTP/1.1\r\nHost: localhost:REDACTED\r\nUser-Agent: Racket/REDACTED (net/http-client)\r\nAccept-Encoding: gzip,deflate\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
    #"HTTP/1.1 200 OK"
    '()
    #""]

   ["PUT" #"frob" "HTTP/1.1 200 OK\r\n\r\n"
    #"PUT / HTTP/1.1\r\nHost: localhost:REDACTED\r\nUser-Agent: Racket/REDACTED (net/http-client)\r\nAccept-Encoding: gzip,deflate\r\nContent-Length: 4\r\nConnection: close\r\n\r\nfrob"
    #"HTTP/1.1 200 OK"
    '()
    #""]

   ["PUT" "frob" "HTTP/1.1 200 OK\r\n\r\n"
    #"PUT / HTTP/1.1\r\nHost: localhost:REDACTED\r\nUser-Agent: Racket/REDACTED (net/http-client)\r\nAccept-Encoding: gzip,deflate\r\nContent-Length: 4\r\nConnection: close\r\n\r\nfrob"
    #"HTTP/1.1 200 OK"
    '()
    #""]

   ["PUT"
    (λ (w) (w "fr") (w "ob"))
    "HTTP/1.1 200 OK\r\n\r\n"
    #"PUT / HTTP/1.1\r\nHost: localhost:REDACTED\r\nUser-Agent: Racket/REDACTED (net/http-client)\r\nAccept-Encoding: gzip,deflate\r\nTransfer-Encoding: chunked\r\nConnection: close\r\n\r\n2\r\nfr\r\n2\r\nob\r\n0\r\n\r\n"
    #"HTTP/1.1 200 OK"
    '()
    #""]

   ["PUT"
    (λ (w) (void))
    "HTTP/1.1 200 OK\r\n\r\n"
    #"PUT / HTTP/1.1\r\nHost: localhost:REDACTED\r\nUser-Agent: Racket/REDACTED (net/http-client)\r\nAccept-Encoding: gzip,deflate\r\nTransfer-Encoding: chunked\r\nConnection: close\r\n\r\n0\r\n\r\n"
    #"HTTP/1.1 200 OK"
    '()
    #""]

   ["HEAD" #f "HTTP/1.1 200 OK\r\n\r\n"
    #"HEAD / HTTP/1.1\r\nHost: localhost:REDACTED\r\nUser-Agent: Racket/REDACTED (net/http-client)\r\nAccept-Encoding: gzip,deflate\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
    #"HTTP/1.1 200 OK"
    '()
    #""])

  ;; Body-less keep alive tests.
  ;; https://datatracker.ietf.org/doc/html/rfc2616#section-10.1
  ;; https://datatracker.ietf.org/doc/html/rfc2616#section-10.2.5
  ;; https://datatracker.ietf.org/doc/html/rfc2616#section-10.3.5
  (let ()
    (define l (tcp-listen 0 128 #t "127.0.0.1"))
    (define-values (_host1 port _host2 _port2)
      (tcp-addresses l #t))
    (with-handlers ([exn:fail? (lambda (e)
                                 (tcp-close l)
                                 (raise e))])
      (thread
       (lambda ()
         (define-values (in out)
           (tcp-accept l))
         (for ([response '("HTTP/1.1 104 Not Real\r\nConnection: keep-alive\r\n"
                           "HTTP/1.1 204 No Content\r\nConnection: keep-alive\r\n"
                           "HTTP/1.1 304 Not Modified\r\nConnection: keep-alive\r\n")])
           (void (read-request in))
           (display response out)
           (fprintf out "\r\n")
           (flush-output out))
         (close-input-port in)
         (close-output-port out)))
      (define hc (hc:http-conn-open "127.0.0.1" #:port port))
      (void
       (sync
        (thread
         (lambda ()
           (for ([expected-status '(#"HTTP/1.1 104 Not Real"
                                    #"HTTP/1.1 204 No Content"
                                    #"HTTP/1.1 304 Not Modified")])
             (let-values ([(status _heads port) (hc:http-conn-sendrecv! hc "/")])
               (check-equal? status expected-status)
               (close-input-port port)))))
        (handle-evt
         (alarm-evt (+ (current-inexact-milliseconds) 5000))
         (lambda (_)
           (fail "timed out")))))))

  (require (prefix-in es: "http-proxy/echo-server.rkt")
           (prefix-in ps: "http-proxy/proxy-server.rkt"))

  (define-values (es:port es:server-thread es:shutdown-server)
    (es:server))

  (define-values (ps:port ps:server-thread ps:shutdown-server)
    (ps:server))

  (check-equal?
    (let-values (([ssl-ctx from to abandon-p]
                  (hc:http-conn-CONNECT-tunnel "localhost" ps:port
                                               "localhost" es:port #:ssl? #f)))
      (fprintf to "MONKEYS\n")
      (abandon-p to)
      (begin0
        (read-line from)
        (abandon-p from)))
    "MONKEYS")

  (let ([c (hc:http-conn)])
    (check-false (hc:http-conn-live? c))
    (check-false (hc:http-conn-liveable? c))

    (hc:http-conn-open! c "localhost"
                        #:port es:port
                        #:ssl? #f
                        #:auto-reconnect? #t)
    (check-true (hc:http-conn-live? c))
    (check-true (hc:http-conn-liveable? c))

    (let-values ([(status headers content-port)
                  (hc:http-conn-sendrecv! c
                                          "/"
                                          #:close? #t
                                          #:data #"BANANAS")])
      (check-false (hc:http-conn-live? c))
      (check-true (hc:http-conn-liveable? c))
      (check-equal? (port->bytes content-port) #"BANANAS"))

    (let-values ([(status headers content-port)
                  (hc:http-conn-sendrecv! c
                                          "/"
                                          #:close? #t
                                          #:data #"MONKEYS")])
      (check-false (hc:http-conn-live? c))
      (check-true (hc:http-conn-liveable? c))
      (check-equal? (port->bytes content-port) #"MONKEYS")))

  (ps:shutdown-server)
  (es:shutdown-server)

  ;; crf: https://github.com/racket/racket/issues/4503
  ;; net/http-client: http-conn-send! and http-conn-recv! use incorrect regexes to parse headers
  (let ()
    (local-require (prefix-in gs: "http-proxy/generic-server.rkt"))
    (define (test-colon-field-lws response-raw)
      (define-values (gs:port gs:thread gs:kill)
        (gs:serve
          (lambda (inp outp)
            (void (read-request inp))
            (display response-raw outp)
            (flush-output outp)
            ; returning will close outp and mask the hang
            (sync))))

      (define c (hc:http-conn-open "localhost" #:port gs:port #:ssl? #f))
      (check-true (hc:http-conn-live? c))
      (define-values (status-line _headers bodyp)
        (hc:http-conn-sendrecv! c "" #:method #"GET" #:headers empty #:close? #t))
      (check-equal? status-line #"HTTP/1.1 200 OK")
      (sync
        (thread (lambda () (check-equal? (port->bytes bodyp) #"MONKEYS")))
        (handle-evt
         (alarm-evt (+ (current-inexact-monotonic-milliseconds) 2000) #t)
         (lambda (_) (fail "timed out"))))
      (void))

    (define cases (list #"HTTP/1.1 200 OK\r\nContent-Length:7\r\n\r\nMONKEYS"
                        #"HTTP/1.1 200 OK\r\nContent-Length:\t\t7\r\n\r\nMONKEYS"
                        #"HTTP/1.1 200 OK\r\nContent-Length:\t   \t        7\r\n\r\nMONKEYS"))

    (for ([raw (in-list cases)])
      (with-check-info (['response-raw raw])
        (test-colon-field-lws raw)))))
