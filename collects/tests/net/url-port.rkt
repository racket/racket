#lang racket
(require net/url
         mzlib/thread
         openssl
         tests/eli-tester)

(define (run-tests scheme wrap-ports)
  (define ((make-tester url->port) response)
    (define port-no 9001)
    (define server-cust
      (make-custodian))
    (parameterize ([current-custodian server-cust])
      (thread 
       (λ ()
          (run-server port-no 
                      (lambda (ip op)
                        (let-values ([(ip op) (wrap-ports ip op)])
                          (regexp-match #rx"(\r\n|^)\r\n" ip)
                          (display response op)
                          (close-output-port op)
                          (close-input-port ip)))
                      +inf.0))))
    (sleep 1)
    (dynamic-wind
        void
        (λ ()
           (port->string
            (url->port
             (url scheme #f "localhost" port-no
                  #t empty empty #f))))
        (λ ()
           (custodian-shutdown-all server-cust))))  

  (define get-pure
    (make-tester get-pure-port))
  (define get-impure
    (make-tester get-impure-port))

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

   (get-impure
    "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n23\r\nThis is the data in the first chunk\r\n1A\r\nand this is the second one\r\n0\r\n")
   =>
   "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n23\r\nThis is the data in the first chunk\r\n1A\r\nand this is the second one\r\n0\r\n"
   
   (get-impure
    "HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\n\r\nThis is the data in the first chunk and this is the second one\r\n")
   =>
   "HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\n\r\nThis is the data in the first chunk and this is the second one\r\n"
   ))

(run-tests "http" values)
(run-tests "https" (let ([ctx (ssl-make-server-context)])
                     (ssl-load-certificate-chain! ctx (collection-file-path "test.pem" "openssl"))
                     (ssl-load-private-key! ctx (collection-file-path "test.pem" "openssl"))
                     (lambda (in out)
                       (ports->ssl-ports in out #:mode 'accept #:context ctx))))

