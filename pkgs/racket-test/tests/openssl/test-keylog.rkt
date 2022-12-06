#lang racket/base

(require net/url
         net/url-connect
         openssl
         racket/runtime-path
         rackunit)

(define (request ctx url)
  (parameterize ([current-custodian (make-custodian)]
                 [current-https-protocol ctx])
    (close-input-port (get-pure-port (string->url url)))))

(define (call-with-unsealed-context proc)
  (define ctx (ssl-make-client-context))
  (ssl-load-default-verify-sources! ctx)
  (ssl-set-verify! ctx #t)
  (ssl-set-verify-hostname! ctx #t)
  (ssl-set-ciphers! ctx "DEFAULT:!aNULL:!eNULL:!LOW:!EXPORT:!SSLv2")
  (proc ctx))

(define ch (make-channel))
(define logger (make-logger))
(void
 (thread
  (lambda ()
    (define receiver
      (make-log-receiver logger 'debug))
    (let loop ()
      (channel-put ch (sync receiver))
      (loop)))))
(define (drain-data)
  (let loop ([data null])
    (sync (system-idle-evt))
    (cond
      [(sync/timeout 0 ch)
       => (λ (d) (loop (cons d data)))]
      [else
       (reverse data)])))

(test-case "client context keylogger"
  (call-with-unsealed-context
   (lambda (ctx)
     (ssl-set-keylogger! ctx logger)
     (request ctx "https://api.github.com/")
     (check-true (positive? (length (drain-data))))

     (ssl-set-keylogger! ctx #f)
     (ssl-seal-context! ctx)
     (request ctx "https://api.github.com/")
     (check-equal? (drain-data) null))))

(test-case "discarded client context does not cause problems"
  (parameterize ([current-custodian (make-custodian)])
    (define-values (in out)
      (call-with-unsealed-context
       (lambda (ctx)
         (ssl-set-keylogger! ctx logger)
         (ssl-seal-context! ctx)
         (define-values (in out)
           (parameterize ([current-custodian (make-custodian)])
             (ssl-connect "www.racket-lang.org" 443 ctx)))
         (check-true (positive? (length (drain-data))))
         (values in out))))
    (collect-garbage)
    (display #"GET / HTTP/1.1\r\nHost: www.racket-lang.org\r\n\r\n" out)
    (flush-output)
    (check-equal? (read-line in 'return-linefeed) "HTTP/1.1 200 OK")
    (sync (system-idle-evt))
    (check-equal? (drain-data) null)
    (custodian-shutdown-all (current-custodian))))

(define-runtime-path server-key "server_key.pem")
(define-runtime-path server-cert "server_lambda_crt.pem")

(test-case "server context retains keylogger"
  (parameterize ([current-custodian (make-custodian)])
    (define listener
      (let ()
        (define ctx (ssl-make-server-context))
        (ssl-load-default-verify-sources! ctx)
        (ssl-set-ciphers! ctx "DEFAULT:!aNULL:!eNULL:!LOW:!EXPORT:!SSLv2")
        (ssl-load-certificate-chain! ctx server-cert)
        (ssl-load-private-key! ctx server-key)
        (ssl-set-keylogger! ctx logger)
        (ssl-seal-context! ctx)
        (ssl-listen 0 512 #t "127.0.0.1" ctx)))
    (collect-garbage)
    (check-equal? (drain-data) null)

    (define-values (host port _remote-host _remote-port)
      (ssl-addresses listener #t))

    (define handler-thd
      (thread
       (lambda ()
         (let loop ()
           (sync
            (handle-evt (thread-receive-evt) void)
            (handle-evt
             listener
             (lambda (l)
               (define-values (in out)
                 (ssl-accept l))
               (write-string (read-line in) out)
               (flush-output out)
               (close-output-port out)
               (close-input-port in)
               (loop))))))))

    (define (ping message)
      (define-values (in out)
        (ssl-connect host port))
      (displayln message out)
      (check-equal? (read-line in) message)
      (close-output-port out)
      (close-input-port in))

    (ping "hello from client 1")
    (check-true (positive? (length (drain-data))))

    (ping "hello from client 2")
    (check-true (positive? (length (drain-data))))

    (thread-send handler-thd '(stop))
    (thread-wait handler-thd)
    (ssl-close listener)
    (check-equal? (drain-data) null)))
