#lang racket/base

(provide delay force)
;; This version of promises does _not_ handle re-entrance detection,
;; exceptions, or multiple value returns.

;; val is a thunk or the eventual value
;; status is whether the thunk has been forced already
(struct promise (val status) #:mutable #:authentic)

(define-syntax-rule (delay e) (promise (lambda () e) #f))

(define (force v)
  (cond
    [(promise? v)
     (define s (promise-status v))
     (cond
       [(not s)
        (define result ((promise-val v)))
        (set-promise-val! v result)
        (set-promise-status! v #t)
        result]
       [else (promise-val v)])]
    [else v]))
