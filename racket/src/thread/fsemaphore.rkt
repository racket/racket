#lang racket/base
(require "check.rkt"
         "parameter.rkt"
         "future-lock.rkt"
         (submod "future.rkt" for-fsemaphore)
         "evt.rkt"
         "sync.rkt")

(provide fsemaphore?
         make-fsemaphore
         fsemaphore-post
         fsemaphore-wait
         fsemaphore-try-wait?
         fsemaphore-count)

(struct fsemaphore ([c #:mutable]          ; counter
                    lock
                    [dependents #:mutable] ; dependent futures
                    [dep-box #:mutable])   ; for waiting by non-futures
  #:authentic)

(struct fsemaphore-box-evt (b)
  #:property prop:evt (poller (lambda (fsb poll-ctx)
                                (define b (fsemaphore-box-evt-b fsb))
                                (cond
                                  [(unbox b) (values '(#t) #f)]
                                  [else (values #f fsb)]))))


(define/who (make-fsemaphore init)
  (check who exact-nonnegative-integer? init)
  (fsemaphore init
              (make-lock)
              #hasheq()
              #f))

(define/who (fsemaphore-post fs)
  (check who fsemaphore? fs)
  (with-lock (fsemaphore-lock fs)
    (define c (fsemaphore-c fs))
    (cond
      [(zero? c)
       (define b (fsemaphore-dep-box fs))
       (define deps (fsemaphore-dependents fs))
       ;; If a future is waiting on the semaphore, it wins over any
       ;; non-future threads that are blocked on the fsemaphore.
       ;; That's not a great choice, but it we don't have to worry
       ;; about keeping track of threads that are in still line versus
       ;; threads that have been interrupted.
       (cond
         [(not (hash-empty? deps))
          (define f (hash-iterate-key deps (hash-iterate-first deps)))
          (set-fsemaphore-dependents! fs (hash-remove deps f))
          (future-notify-dependent f)]
         [else
          (set-fsemaphore-c! fs 1)
          (when b
            ;; This is a kind of broadcast wakeup, and then the
            ;; awakened threads will compete for the fsemaphore:
            (set-fsemaphore-dep-box! fs #f)
            (set-box! b #t)
            (wakeup-this-place))])]
      [else
       (set-fsemaphore-c! fs (add1 c))])))

(define/who (fsemaphore-wait fs)
  (check who fsemaphore? fs)
  (lock-acquire (fsemaphore-lock fs))
  (define c (fsemaphore-c fs))
  (cond
    [(zero? c)
     (define me-f (current-future))
     (cond
       [me-f
        (lock-acquire (future*-lock me-f))
        (set-fsemaphore-dependents! fs (hash-set (fsemaphore-dependents fs) me-f #t))
        (set-future*-state! me-f 'fsema)
        (lock-release (fsemaphore-lock fs))
        (future-suspend) ; expects lock on f and releases it
        (void)]
       [else
        (define dep-box (or (fsemaphore-dep-box fs)
                            (let ([b (box #f)])
                              (set-fsemaphore-dep-box! fs b)
                              b)))
        (lock-release (fsemaphore-lock fs))
        (sync (fsemaphore-box-evt dep-box))
        (fsemaphore-wait fs)])]
    [else
     (set-fsemaphore-c! fs (sub1 c))
     (lock-release (fsemaphore-lock fs))]))

(define/who (fsemaphore-try-wait? fs)
  (check who fsemaphore? fs)
  (with-lock (fsemaphore-lock fs)
    (define c (fsemaphore-c fs))
    (cond
      [(zero? c) #f]
      [else
       (set-fsemaphore-c! fs (sub1 c))
       #t])))

(define/who (fsemaphore-count fs)
  (check who fsemaphore? fs)
  (with-lock (fsemaphore-lock fs)
    (fsemaphore-c fs)))
