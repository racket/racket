#lang racket/base
(provide serve
         current-conn-timeout)

(require mzlib/thread
         racket/tcp)

(define current-conn-timeout (make-parameter #f))

(define (serve conn-proc)
  ;; use of channel `ch` allows us to wait until the server is
  ;; listening before continuing -- needed for test suites that “just
  ;; want to get on with it”
  (define ch (make-channel))
  (define t (thread
             (λ ()
               (run-server #f
                           conn-proc
                           (current-conn-timeout)
                           void ; handler
                           (λ (a-false [max-allow-wait 4] [reuse? #f] [hostname #f])
                             (define listener
                               (tcp-listen 0 max-allow-wait reuse? hostname))
                             (define-values (_0 the-port _1 _2)
                               (tcp-addresses listener #t))
                             (channel-put ch the-port)
                             listener)))))
  (values (channel-get ch) t (λ () (kill-thread t))))

;; tested via the echo-server (in this directory)
;; (module+ test)
