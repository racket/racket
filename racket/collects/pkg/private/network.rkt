#lang racket/base
(require net/url
         racket/format
         "print.rkt"
         "config.rkt")

(provide call-with-network-retries
         call/input-url+200
         exn:fail:can-retry)

(struct exn:fail:can-retry exn:fail ())

(define NETWORK-INITIAL-PAUSE 0.1)

;; Retry `thunk` on any `exn:fail:network` exception. A fresh
;; custodian is in place during the call to `thunk`, so resources
;; are reliably cleaned up (and cannt be allocated and returned
;; by `thunk`, except by using a different custodian).
(define (call-with-network-retries thunk)
  (define retry-count (get-network-retries))
  (let loop ([retries 0] [pause-time NETWORK-INITIAL-PAUSE])
    (define (maybe-retry exn)
      (cond
       [(retries . >= . retry-count)
        (raise exn)]
       [else
        ;; Pause, then try again
        (log-pkg-info "Network error; retrying after ~as"
                      pause-time)
        (sleep pause-time)
        (loop (add1 retries) (* 2 pause-time))]))
    (with-handlers* ([exn:fail:network? maybe-retry]
                     [exn:fail:can-retry? maybe-retry])
      (define c (make-custodian))
      (parameterize ([current-custodian c])
        (dynamic-wind
         void
         thunk
         (lambda ()
           (custodian-shutdown-all c)))))))

(define success-codes '(200))
(define not-found-codes '(404 410))

(define other-retry-codes '(408)) ; not counting 5xx
(define (retry-code? c)
  (or (and (integer? c) (<= 500 c 599))
      (memv c other-retry-codes)))

(define (call/input-url+200 url handler
                            #:who [who 'download]
                            #:auto-retry? [auto-retry? #t]
                            #:headers [headers '()]
                            #:not-found-handler [not-found-handler (lambda (s) #f)])
  ((if auto-retry?
       call-with-network-retries
       (lambda (f) (f)))
   (lambda ()
     (define-values (p hs)
       (get-pure-port/headers url headers
                              #:redirections 25
                              #:status? #t))
     (define status (string->number (substring hs 9 12)))
     (cond
      [(memv status success-codes)
       (begin0
        (handler p)
        (close-input-port p))]
      [(memv status not-found-codes)
       (close-input-port p)
       (not-found-handler hs)]
      [else
       (raise ((if (retry-code? status) exn:fail:can-retry exn:fail)
               (format (~a "~a: error from server\n"
                           "  URL: ~a\n"
                           "  status code: ~a")
                       who
                       (url->string url)
                       status)
               (current-continuation-marks)))]))))
