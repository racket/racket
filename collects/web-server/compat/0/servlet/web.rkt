#lang racket/base
(require (prefix-in new: web-server/servlet/web)
         "../http/response-structs.rkt")

(define-syntax-rule (define-send/back-like new:send/back send/back)
  (define (send/back r)
    (new:send/back (normalize-response r))))

(define-send/back-like new:send/back send/back)
(define-send/back-like new:send/finish send/finish)

(define-syntax-rule (define-send/forward-like new:send/forward send/forward)
  (define (send/forward generator)
    (new:send/forward
     (λ (k-url)
       (normalize-response (generator k-url))))))

(define-send/forward-like new:send/forward send/forward)
(define-send/forward-like new:send/suspend send/suspend)
(define-send/forward-like new:send/suspend/url send/suspend/url)

(define-syntax-rule (define-ssd-like new:send/suspend/dispatch send/suspend/dispatch)
  (define (send/suspend/dispatch generator)
    (new:send/suspend/dispatch
     (λ (embed/url)
       (normalize-response
        (generator embed/url))))))

(define-ssd-like new:send/suspend/dispatch send/suspend/dispatch)
(define-ssd-like new:send/suspend/url/dispatch send/suspend/url/dispatch)

(provide 
 (rename-out [new:servlet-prompt servlet-prompt]
             [new:continuation-url? continuation-url?]
             [new:current-servlet-continuation-expiration-handler
              current-servlet-continuation-expiration-handler]
             [new:redirect/get redirect/get]
             [new:redirect/get/forget redirect/get/forget]
             [new:adjust-timeout! adjust-timeout!]
             [new:clear-continuation-table! clear-continuation-table!]
             [new:with-errors-to-browser with-errors-to-browser])
 (all-defined-out))