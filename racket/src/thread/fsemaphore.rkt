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

(struct fsemaphore ([c #:mutable] ; counter
                    lock
                    [queue-head #:mutable]
                    [queue-tail #:mutable])
  #:authentic)

(define/who (make-fsemaphore init)
  (check who exact-nonnegative-integer? init)
  (fsemaphore init
              (make-lock)
              null
              null))

(define/who (fsemaphore-post fs)
  (check who fsemaphore? fs)
  (with-lock (fsemaphore-lock fs)
    (define c (fsemaphore-c fs))
    (cond
      [(zero? c)
       (let loop ()
         (define queue-head (fsemaphore-queue-head fs))
         (cond
           [(pair? queue-head)
            (define f (car queue-head))
            (set-fsemaphore-queue-head! fs (cdr queue-head))
            (unless (future-notify-dependent f)
              ;; future for `thread/parallel` was terminated or suspended; try next
              (loop))]
           [(pair? (fsemaphore-queue-tail fs))
            (set-fsemaphore-queue-head! fs (reverse (fsemaphore-queue-tail fs)))
            (set-fsemaphore-queue-tail! fs null)
            (loop)]
           [else
            (set-fsemaphore-c! fs 1)]))]
      [else
       (set-fsemaphore-c! fs (add1 c))])))

(define/who (fsemaphore-wait fs)
  (check who fsemaphore? fs)
  (future-unblock) ; in case we should be a in a future pthread, but aren't
  (lock-acquire (fsemaphore-lock fs))
  (define c (fsemaphore-c fs))
  (cond
    [(zero? c)
     (define me-f (current-future))
     (cond
       [me-f
        (lock-acquire (future*-lock me-f))
        (set-fsemaphore-queue-tail! fs (cons me-f (fsemaphore-queue-tail fs)))
        (future-maybe-notify-stop me-f)
        (set-future*-state! me-f 'fsema)
        (lock-release (fsemaphore-lock fs))
        (future-suspend) ; expects lock on me-f and releases it
        (void)]
       [else
        (lock-release (fsemaphore-lock fs))
        (call-in-future (lambda () (fsemaphore-wait fs)))])]
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
