#lang racket/base
(require net/url
         "print.rkt"
         "config.rkt")

(provide call-with-network-retries
         call/input-url+200)

(define NETWORK-INITIAL-PAUSE 0.1)

;; Retry `thunk` on any `exn:fail:network` exception. A fresh
;; custodian is in place during the call to `thunk`, so resources
;; are reliably cleaned up (and cannt be allocated and returned
;; by `thunk`, except by using a different custodian).
(define (call-with-network-retries thunk)
  (define retry-count (get-network-retries))
  (let loop ([retries 0] [pause-time NETWORK-INITIAL-PAUSE])
    (with-handlers* ([exn:fail:network? (lambda (exn)
                                          (cond
                                           [(retries . >= . retry-count)
                                            (raise exn)]
                                           [else
                                            ;; Pause, then try again
                                            (log-pkg-info "Network error; retrying after ~as"
                                                          pause-time)
                                            (sleep pause-time)
                                            (loop (add1 retries) (* 2 pause-time))]))])
      (define c (make-custodian))
      (parameterize ([current-custodian c])
        (dynamic-wind
         void
         thunk
         (lambda ()
           (custodian-shutdown-all c)))))))

(define (call/input-url+200 u fun
                            #:auto-retry? [auto-retry? #t]
                            #:headers [headers '()]
                            #:failure [fail-k (lambda (s) #f)])
  ((if auto-retry?
       call-with-network-retries
       (lambda (f) (f)))
   (lambda ()
     #;(printf "\t\tReading ~a\n" (url->string u))
     (define-values (ip hs) (get-pure-port/headers u headers
                                                   #:redirections 25
                                                   #:status? #t))
     (if (string=? "200" (substring hs 9 12))
         (begin0
          (fun ip)
          (close-input-port ip))
         (fail-k hs)))))
