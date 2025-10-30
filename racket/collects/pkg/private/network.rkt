#lang racket/base
(require net/url
         net/url-connect
         net/head
         racket/format
         "print.rkt"
         "config.rkt"
         "timeout.rkt")

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
(define not-modified-codes '(304))

(define other-retry-codes '(408 409)) ; not counting 5xx
(define (retry-code? c)
  (or (and (integer? c) (<= 500 c 599))
      (memv c other-retry-codes)))

(define (call/input-url+200 url handler
                            #:who [who 'download]
                            #:auto-retry? [auto-retry? #t]
                            #:headers [headers '()]
                            #:not-found-handler [not-found-handler (lambda (s) #f)]
                            #:get-etag? [get-etag? #f] ; if #t, then `handler` receives an etag argument
                            #:if-none-match-etag [none-match-etag #f]
                            #:if-none-match-handler [none-match-handler (lambda () #f)])
  ((if auto-retry?
       call-with-network-retries
       (lambda (f) (f)))
   (lambda ()
     (call-in-pkg-timeout-sandbox
      (lambda ()
        (define-values (p hs)
          (parameterize ([current-https-protocol (if (getenv "PLT_PKG_SSL_NO_VERIFY")
                                                     (current-https-protocol)
                                                     'secure)])
            (get-pure-port/headers url
                                   (if none-match-etag
                                       (cons (format "If-None-Match: \"~a\"" none-match-etag)
                                             headers)
                                       headers)
                                   #:redirections 25
                                   #:status? #t)))
        (define status (string->number (substring hs 9 12)))
        (define etag (and get-etag?
                          (let ([s (extract-field "etag" hs)])
                            (and s
                                 (let ([m (regexp-match #rx"(?:W/)?\"([\u21\u23-\u7E\u80-\uFF]*)\"" s)])
                                   (and m (cadr m)))))))
        (cond
          [(memv status success-codes)
           (begin0
             (if get-etag?
                 (handler p etag)
                 (handler p))
             (close-input-port p))]
          [(memv status not-modified-codes)
           (close-input-port p)
           (none-match-handler)]
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
                   (current-continuation-marks)))]))))))
