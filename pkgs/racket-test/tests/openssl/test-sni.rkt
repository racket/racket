#lang racket
(require openssl
         rackunit
         racket/tcp
         racket/runtime-path)

#|
"openssl s_server" prints a "Hostname in TLS extension: <hostname>"
line when the client uses SNI. BUT it only seems to print it when run
interactively (tty). (stupid openssl...)

So use "-tlsextdebug" and use small hostnames so that they're
recognizable despite the "pretty" formatting.
|#

(define-runtime-path server-key "server_key.pem")
(define-runtime-path server-crt "server_crt.pem")
(define-runtime-path server-crt2 "server_crt2.pem")

(define MSG "Hello. This is Racket speaking.\n")

;; server listens on localhost:PORT+counter
;; (need to change port, otherwise get "Address still in use")
(define PORT 4433)
(define counter 0)

(define (go hostname connect)
  (set! counter (add1 counter))
  ;; Set up server
  (define srvout (open-output-string))
  (define-values (_srvout _srvin srvpid _srverr srvctl)
    (apply values
      (process* "/usr/bin/openssl" "s_server"
                "-tlsextdebug"
                "-accept" (number->string (+ PORT counter))
                "-cert" server-crt "-key" server-key
                "-cert2" server-crt2 "-key2" server-key "-servername" "test.com")))
  (sleep 0.2) ;; wait for server to bind the port
  (define buf (make-bytes #e1e4))
  (define buflen (read-bytes-avail! buf _srvout))
  ;; (eprintf "s_server says: ~s bytes: ~s\n" buflen (subbytes buf 0 buflen))

  ;; Make client connection
  (define-values (cin cout) (connect (+ PORT counter)))
  (display MSG cout)
  ;; Close client connection
  (close-output-port cout)
  (close-input-port cin)

  ;; FIXME: create a read-until-silent-for-nsecs helper fun?

  ;; Shut down server
  (sleep 0.1)
  (define buflen2 (read-bytes-avail! buf _srvout))
  (define stext (bytes->string/utf-8 (subbytes buf 0 buflen2)))
  (srvctl 'interrupt)
  (srvctl 'kill)
  ;; (eprintf "s_server says(2): ~s bytes: ~s\n" buflen2 stext)

  ;; Check server output
  ;; - Check for SNI extension output
  (when hostname
    (check-regexp-match
     (string-append "TLS client extension \"server name\".*"
                    (regexp-quote hostname))
     stext))
  ;; - Check for msg output (sanity check)
  (check-regexp-match (regexp-quote MSG)
                      stext)
  (void))

;; ssl-connect automatically passes along hostname  (for tls, not sslv3)
(test-case "TLS ssl-connect localhost"
  (go "localhost" (lambda (port) (ssl-connect "localhost" port 'tls))))

;; check alternate hostname by using ports->ssl-ports
(define ((make-connect/hostname protocol hostname) port)
  (define-values (in out) (tcp-connect "localhost" port))
  (ports->ssl-ports in out #:encrypt protocol #:hostname hostname))

(test-case "TLS w/ SNI, #1"
  (go "test.com" (make-connect/hostname 'tls "test.com")))
(test-case "TLS w/ SNI, #2"
  (go "another.org" (make-connect/hostname 'tls "another.org")))
(test-case "TLS w/o SNI"
  (go #f (make-connect/hostname 'tls #f)))
