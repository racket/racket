#lang racket/base
(require "os-thread.rkt"
         "schedule.rkt"
         "atomic.rkt")

(provide os-async-channel?
         make-os-async-channel
         os-async-channel-put
         os-async-channel-try-get
         os-async-channel-get)

(struct os-async-channel (head tail lock ready signal)
  #:mutable
  #:property prop:evt (unsafe-poller
                       (lambda (self wakeups)
                         (define vals (try-get self (not wakeups)))
                         (if vals
                             (values vals #f)
                             (values #f self)))))

;; In atomic mode, if in a Racket thread
(define (try-get ac dequeue?)
  (os-semaphore-wait (os-async-channel-lock ac))
  (when (and (null? (os-async-channel-head ac))
             (pair? (os-async-channel-tail ac)))
    (set-os-async-channel-head! ac (reverse (os-async-channel-tail ac)))
    (set-os-async-channel-tail! ac null))
  (define head (os-async-channel-head ac))
  (when (and dequeue? (pair? head))
    (set-os-async-channel-head! ac (cdr head)))
  (os-semaphore-post (os-async-channel-lock ac))
  (if (pair? head)
      (list (car head))
      #f))

(define (make-os-async-channel)
  (unless os-thread-enabled?
    (raise (exn:fail:unsupported "make-os-async-channel: OS threads are not supported"
                                 (current-continuation-marks))))
  (define sema (make-os-semaphore))
  (os-semaphore-post sema)
  (os-async-channel null null sema (make-os-semaphore) (unsafe-make-signal-received)))

;; Works in any thread:
(define (os-async-channel-put ac v)
  (unless (os-async-channel? ac)
    (raise-argument-error 'os-async-channel-put "os-async-channel?" ac))
  (start-atomic) ; allowed even in non-Racket threads
  (os-semaphore-wait (os-async-channel-lock ac))
  (set-os-async-channel-tail! ac (cons v (os-async-channel-tail ac)))
  (os-semaphore-post (os-async-channel-lock ac))
  ;; in case a non-Racket thread is waiting:
  (os-semaphore-post (os-async-channel-ready ac))
  ;; in case scheduler is waiting in Racket thread:
  ((os-async-channel-signal ac))
  (end-atomic)) ; allowed even in non-Racket threads

;; Works in any thread:
(define (os-async-channel-try-get ac [default #f])
  (unless (os-async-channel? ac)
    (raise-argument-error 'os-async-channel-try-get "os-async-channel?" ac))
  (define vals (try-get ac #t))
  (if vals
      (car vals)
      default))

;; Works only in a non-Racket thread:
(define (os-async-channel-get ac)
  (unless (os-async-channel? ac)
    ;; This is just a fancy way to crash, since raising exceptions in
    ;; a non-Racket thread will not work:
    (raise-argument-error 'os-async-channel-get "os-async-channel?" ac))
  (let loop ()
    (define vals (try-get ac #t))
    (cond
      [vals (car vals)]
      [else
       (os-semaphore-wait (os-async-channel-ready ac))
       (loop)])))
