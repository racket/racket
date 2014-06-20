#lang racket
(require (for-syntax racket/syntax))
(require (prefix-in r: racket/async-channel))

;; all the functions from racket/async-channel, but wrapped to hide contracts

;; create "r:" prefixed identifier
(define-for-syntax (r: id) (format-id id "r:~a" id))

;; eta expand to hide contracts
(define-syntax (provide/eta stx)
  (syntax-case stx ()
    [(_ f ...)
     (with-syntax ([(r:f ...) (map r: (syntax->list #'(f ...)))])
       #'(begin 
           (define (f . xs) (apply r:f xs)) ... 
           (provide f ...)))]))

(provide/eta async-channel?
             make-async-channel
             async-channel-get
             async-channel-try-get
             async-channel-put
             async-channel-put-evt)
