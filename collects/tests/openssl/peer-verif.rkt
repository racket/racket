#lang racket

(require openssl
         ffi/unsafe
         racket/tcp)

(define (check fmt got expect)
  (unless (equal? got expect)
    (error 'check fmt got)))

(define ssl-server-context (ssl-make-server-context 'sslv3))


(ssl-load-private-key! ssl-server-context "server_key.pem")
(ssl-load-certificate-chain! ssl-server-context "server_crt.pem")
(ssl-load-verify-root-certificates! ssl-server-context "cacert.pem")
(ssl-try-verify! ssl-server-context #t)

(define ssl-listener (ssl-listen 55000
                                 4
                                 #f
                                 "127.0.0.1"
                                 ssl-server-context))

(define listener-main 
  (thread 
   (lambda()
     (let-values ([(in out) (ssl-accept ssl-listener)])
       (check "Server: Accepted connection.~n" #t #t)
       (check "Server: Verified ~v~n" (ssl-peer-verified? in) #t)
       (check "Server: Verified ~v~n" (ssl-peer-verified? out) #t)
       (check "Server: Verified Peer Subject Name ~v~n" (ssl-peer-subject-name in)
              #"/CN=testclient.okcomps.com/ST=OH/C=US/emailAddress=root@okcomps.com/O=OK Computers LLC/OU=IT")
       (check "Server: Verified Peer Issuer Name ~v~n" (ssl-peer-issuer-name in)
              #"/CN=okcomps.com/ST=OH/C=US/emailAddress=root@okcomps.com/O=OK Computers LLC/OU=IT Department")
       (ssl-close ssl-listener)
       (check "Server: From Client: ~a~n" (read-line in) "yay the connection was made")
       (close-input-port in)
       (close-output-port out)))))


(define ssl-client-context (ssl-make-client-context 'sslv3))

(ssl-load-private-key! ssl-client-context "client_key.pem")

;connection will still proceed if these methods aren't called
;change to #f to try it
(when #t
  (ssl-load-certificate-chain! ssl-client-context "client_crt.pem")
  (ssl-load-verify-root-certificates! ssl-client-context "cacert.pem")
  (ssl-set-verify! ssl-client-context #t))


(let-values ([(in out) (ssl-connect "127.0.0.1"
                                     55000
                                     ssl-client-context)])
  (check "Client: Made connection.~n" #t #t)
  (check "Client: Verified ~v~n" (ssl-peer-verified? in) #t)
  (check "Client: Verified ~v~n" (ssl-peer-verified? out) #t)
  (check "Client: Verified Peer Subject Name ~v~n" (ssl-peer-subject-name in)
         #"/CN=test.okcomps.com/ST=OH/C=US/emailAddress=root@okcomps.com/O=OK Computers LLC/OU=IT")
  (check "Client: Verified Peer Issuer Name ~v~n" (ssl-peer-issuer-name in)
         #"/CN=okcomps.com/ST=OH/C=US/emailAddress=root@okcomps.com/O=OK Computers LLC/OU=IT Department")
  (write-string (format "yay the connection was made~n") out)
  (close-input-port in)
  (close-output-port out))


(thread-wait listener-main)

;certificate revocation list
;enables denial of connections that provide a certificate on the given certificate revocation list
