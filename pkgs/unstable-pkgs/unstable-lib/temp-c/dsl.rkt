#lang racket/base
(require racket/match
         racket/stxparam
         (for-syntax racket/base)
         "monitor.rkt"
         unstable/automata/machine
         unstable/automata/re
         unstable/automata/re-ext)
(provide call ret with-monitor label
         re->monitor-predicate/concurrent
         re->monitor-predicate/serial
         (all-from-out "monitor.rkt"
                       unstable/automata/re
                       unstable/automata/re-ext))

(define-syntax-parameter stx-monitor-id
  (λ (stx) (raise-syntax-error 'label "Used outside monitor" stx)))

(define-syntax-rule (label n K)
  (monitor/c stx-monitor-id n K))

(define-syntax with-monitor
  (syntax-rules ()
    [(_ K)
     (let ([monitor (λ (x) #t)])
       (syntax-parameterize ([stx-monitor-id (make-rename-transformer #'monitor)])
                            K))]
    [(_ K T)
     (let ([monitor (re->monitor-predicate/serial (re T))])
       (syntax-parameterize ([stx-monitor-id (make-rename-transformer #'monitor)])
                            K))]
    [(_ K #:concurrent T)
     (let ([monitor (re->monitor-predicate/concurrent (re T))])
       (syntax-parameterize ([stx-monitor-id (make-rename-transformer #'monitor)])
                            K))]))

(define (re->monitor-predicate/concurrent m)
  (define inner-accepts?
    (re->monitor-predicate/serial m))
  (define t
    (thread 
     (λ ()
       (let loop ()
         (define m (thread-receive))
         (define evt (car m))
         (define qt (cdr m))
         (thread-resume qt (current-thread))
         (thread-send qt (inner-accepts? evt)
                      (λ () (error 'monitor "Failed to contact requester")))
         (loop)))))
  (define (accepts? evt)
    (thread-resume t (current-thread))
    (thread-send t (cons evt (current-thread))
                 (λ () (error 'monitor "Failed to contact monitor")))
    (thread-receive))
  accepts?)

(define (re->monitor-predicate/serial m)
  (define current-re m)
  (λ (evt)
    #;(printf "~v\n" evt)
    (set! current-re (current-re evt))
    (machine-accepting? current-re)))

(define-match-expander call
  (syntax-rules ()
    [(_ n p ...)
     (monitor:call n _ _ _ _ _ (list p ...))]))

(define-match-expander ret
  (syntax-rules ()
    [(_ n p ...)
     (monitor:return n _ _ _ _ _ _ (list p ...))]))
