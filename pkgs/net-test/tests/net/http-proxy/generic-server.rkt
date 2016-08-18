#lang racket/base
(provide serve
         current-listen-port
         current-conn-timeout)

(require mzlib/thread
         racket/tcp)

(define current-listen-port (make-parameter 12345))

(define current-conn-timeout (make-parameter #f))

(define (serve conn-proc)
  ;; use of semaphore `s` allows us to wait until the server is listening before continuing
  ;; -- needed for test suites that “just want to get on with it”
  (define s (make-semaphore 0))
  (define t (thread
             (λ ()
               (run-server (current-listen-port)
                           conn-proc
                           (current-conn-timeout)
                           void ; handler
                           (λ (port-no
                               (max-allow-wait 4)
                               (reuse? #f)
                               (hostname #f))
                             (dynamic-wind
                              void
                              (λ () (tcp-listen port-no max-allow-wait reuse? hostname))
                              (λ () (semaphore-post s))))))))
  (semaphore-wait s)
  (values t (λ () (kill-thread t))))

;; tested via the echo-server (in this directory)
;; (module+ test)
