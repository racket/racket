#lang racket
(require net/url
         mzlib/thread
         openssl
         tests/eli-tester)

(define (run-tests scheme wrap-ports skip-actual-redirect?)
  (define ((make-tester url->port) response . responses)
    (define server-cust
      (make-custodian))
    (define startup-ch
      (make-channel))
    (define listener-ports
      (make-vector (add1 (length responses))))
    (define (resolve-response response)
      (if (procedure? response)
          (response listener-ports)
          response))
    (parameterize ([current-custodian server-cust])
      (for ([response (in-list (cons response responses))]
            [i (in-naturals)])
        (thread
         (λ ()
           (with-handlers ([exn:fail?
                            (λ (e)
                              (channel-put startup-ch e)
                              (void))])
             (run-server 0
                         (lambda (ip op)
                           (let-values ([(ip op) (wrap-ports ip op)])
                             (regexp-match #rx"(\r\n|^)\r\n" ip)
                             (display (resolve-response response) op)
                             (close-output-port op)
                             (close-input-port ip)))
                         +inf.0
                         void
                         (λ (_port-no [max-allow-wait 4] [reuse? #f] [hostname #f])
                           (define listener
                             (tcp-listen 0 max-allow-wait reuse? hostname))
                           (define-values (_local-addr port-no _remote-addr _remote-port)
                             (tcp-addresses listener #t))
                           (channel-put startup-ch (cons i port-no))
                           listener)
                         tcp-close
                         tcp-accept
                         tcp-accept)))))
    (for ([_ (in-vector listener-ports)])
      (match (sync/timeout 30 startup-ch)
        [#f
         (error 'run-tests "timed out waiting for server startup")]
        [(cons i port-no)
         (vector-set! listener-ports i port-no)]
        [(? exn:fail? e)
         (raise e)]))
    (dynamic-wind
        void
        (λ ()
          (call-with-values
              (λ ()
                (url->port
                 (url scheme #f "localhost" (vector-ref listener-ports 0)
                       #t empty empty #f)))
            (λ vals (apply values (cons (port->string (car vals)) (cdr vals))))))
        (λ ()
          (custodian-shutdown-all server-cust)))))

  (define get-pure
    (make-tester get-pure-port))
  (define get-impure
    (make-tester get-impure-port))
  (define get-pure/redirect
    (make-tester (λ (x) (get-pure-port x #:redirections 1))))
  (define get-pure/headers
    (make-tester get-pure-port/headers))
  (define get-pure/headers/redirect
    (make-tester (λ (x) (get-pure-port/headers x #:redirections 1))))
  (define put-pure
    (make-tester (lambda (url) (put-pure-port url #"data"))))
  (define put-impure
    (make-tester (lambda (url) (put-impure-port url #"data"))))

  (test
   (get-pure
    "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n24\r\nThis is the data in the first chunk \r\n1A\r\nand this is the second one\r\n0\r\n")
   =>
   "This is the data in the first chunk and this is the second one"

   (get-pure
    "HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\n\r\nThis is the data in the first chunk and this is the second one")
   =>
   "This is the data in the first chunk and this is the second one"

   (get-pure
    "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n20\r\nThis is the data in the first ch\r\n21\r\nand this is the second oneXXXXXXX\r\n0\r\n")
   =>
   "This is the data in the first chand this is the second oneXXXXXXX"

   (get-pure/redirect
    "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n24\r\nThis is the data in the first chunk \r\n1A\r\nand this is the second one\r\n0\r\n")
   =>
   "This is the data in the first chunk and this is the second one"

   (get-pure/redirect
    "HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\n\r\nThis is the data in the first chunk and this is the second one")
   =>
   "This is the data in the first chunk and this is the second one"

   (get-pure/redirect
    "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n20\r\nThis is the data in the first ch\r\n21\r\nand this is the second oneXXXXXXX\r\n0\r\n")
   =>
   "This is the data in the first chand this is the second oneXXXXXXX"

   (get-impure
    "HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\n\r\nThis is the data in the first chunk and this is the second one\r\n")
   =>
   "HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\n\r\nThis is the data in the first chunk and this is the second one\r\n"

   (get-pure/headers
    "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n24\r\nThis is the data in the first chunk \r\n1A\r\nand this is the second one\r\n0\r\n")
   =>
   (values "This is the data in the first chunk and this is the second one"
           "Content-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n")

   (get-pure/headers
    "HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\n\r\nThis is the data in the first chunk and this is the second one")
   =>
   (values "This is the data in the first chunk and this is the second one"
           "Content-Type: text/plain\r\n\r\n")

   (get-pure/headers
    "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n20\r\nThis is the data in the first ch\r\n21\r\nand this is the second oneXXXXXXX\r\n0\r\n")
   =>
   (values "This is the data in the first chand this is the second oneXXXXXXX"
           "Content-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n")

   (get-pure/headers
    "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\nAnother-Header: ta-daa\r\n\r\n20\r\nThis is the data in the first ch\r\n21\r\nand this is the second oneXXXXXXX\r\n0\r\n")
   =>
   (values "This is the data in the first chand this is the second oneXXXXXXX"
           "Content-Type: text/plain\r\nTransfer-Encoding: chunked\r\nAnother-Header: ta-daa\r\n\r\n")
   )

  (unless skip-actual-redirect?
    (test
     (get-pure/redirect
      (λ (listener-ports)
        (format "HTTP/1.1 301 Moved Permanently\r\nLocation: http://localhost:~a/whatever\r\n\r\nstuff"
                (vector-ref listener-ports 1)))
      (string-append
       "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n"
       "24\r\nThis is the data in the first chunk \r\n1A\r\nand this is the second one\r\n0\r\n"))

     (get-pure/headers/redirect
      "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\nAnother-Header: ta-daa\r\n\r\n20\r\nThis is the data in the first ch\r\n21\r\nand this is the second oneXXXXXXX\r\n0\r\n")
     =>
     (values "This is the data in the first chand this is the second oneXXXXXXX"
             "Content-Type: text/plain\r\nTransfer-Encoding: chunked\r\nAnother-Header: ta-daa\r\n\r\n")))

  (test
   (put-pure
    "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n24\r\nThis is the data in the first chunk \r\n1A\r\nand this is the second one\r\n0\r\n")
   =>
   "This is the data in the first chunk and this is the second one"
   (put-impure
    "HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\n\r\nThis is the data in the first chunk and this is the second one\r\n")
   =>
   "HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\n\r\nThis is the data in the first chunk and this is the second one\r\n"))

(provide tests)
(module+ main (tests))
(define (tests)
  (test
   (for ([i 100]) ; repeat to catch port leaks
     (run-tests "http" values #f))
   (for ([i 100])
     (run-tests "https" (let ([ctx (ssl-make-server-context)])
                          (ssl-load-certificate-chain! ctx (collection-file-path "test.pem" "openssl"))
                          (ssl-load-private-key! ctx (collection-file-path "test.pem" "openssl"))
                          (lambda (in out)
                            (ports->ssl-ports in out #:mode 'accept #:context ctx)))
                #t))))

(module+ test
  (module config info
    (define timeout 300))
  (require (submod ".." main))) ; for raco test & drdr
