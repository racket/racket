#lang racket/base

(require racket/bool racket/match racket/async-channel)

(provide make-worker-thread worker-thread? worker-thread-working? worker-thread-waiting?
         worker-thread-put worker-thread-try-put
         worker-thread-get worker-thread-try-get
         worker-thread-wait
         worker-thread-send
         worker-thread-kill)

(struct worker-thread (state message-channel result-channel thread) #:mutable #:transparent)
(struct values-result (value-list) #:transparent)
(struct error-result (value) #:transparent)

(define (make-worker-thread handle-message)
  (define msg-ch (make-channel))
  (define res-ch (make-async-channel))
  (define (loop)
    (with-handlers ([(λ (e) #t)  (λ (e) (async-channel-put res-ch (error-result e)))])
      (define msg (channel-get msg-ch))
      (define res (call-with-values (λ () (handle-message msg))
                                    (λ value-list (values-result value-list))))
      (async-channel-put res-ch res))
    (loop))
  (worker-thread 'waiting msg-ch res-ch (thread loop)))

(define (worker-thread-working? r)
  (symbol=? (worker-thread-state r) 'working))

(define (worker-thread-waiting? r)
  (symbol=? (worker-thread-state r) 'waiting))

(define (worker-thread-get* r get fail-thunk)
  (match-define (worker-thread state msg-ch res-ch th) r)
  (case state
    [(working)  (define res (get res-ch))
                (when res (set-worker-thread-state! r 'waiting))
                (match res
                  [(values-result value-list)  (apply values value-list)]
                  [(error-result value)        (raise value)]
                  [#f  (fail-thunk)])]
    [(waiting)  (fail-thunk)]))

(define (worker-thread-try-get r [fail-thunk (λ () #f)])
  (worker-thread-get* r async-channel-try-get fail-thunk))

(define (worker-thread-get-fail)
  (error 'worker-thread-get "cannot get a value from a waiting worker thread"))

(define (worker-thread-get r [fail-thunk worker-thread-get-fail])
  (worker-thread-get* r async-channel-get fail-thunk))

(define (worker-thread-wait r)
  (when (worker-thread-working? r) (worker-thread-get r))
  (void))

(define (worker-thread-put* r msg fail-thunk)
  (match-define (worker-thread state msg-ch res-ch th) r)
  (case state
    [(waiting)  (channel-put msg-ch msg)
                (set-worker-thread-state! r 'working)
                #t]
    [(working)  (fail-thunk)]))

(define (worker-thread-try-put r msg [fail-thunk (λ () #f)])
  (worker-thread-put* r msg fail-thunk))

(define (worker-thread-put-fail)
  (error 'worker-thread-put "cannot send a message to a working worker thread"))

(define (worker-thread-put r msg [fail-thunk worker-thread-put-fail])
  (worker-thread-put* r msg fail-thunk))

(define (worker-thread-send r msg)
  (worker-thread-wait r)
  (worker-thread-put r msg)
  (worker-thread-get r))

(define (worker-thread-kill r)
  (match-define (worker-thread state msg-ch res-ch th) r)
  (kill-thread th))
