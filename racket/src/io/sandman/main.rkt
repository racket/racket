#lang racket/base
(require "../host/place-local.rkt"
         "../../thread/sandman-struct.rkt"
         "../common/internal-error.rkt"
         "../host/thread.rkt"
         "../host/rktio.rkt"
         "lock.rkt")

;; Create an extended sandman that can sleep with a rktio poll set. An
;; external-event set might be naturally implemented with a poll set,
;; except that poll sets are single-use values. So, an external-event
;; set is instead implemented as a tree of callbacks to registers with
;; a (fresh) poll set each time.

;; This sandman builds on the default one to handles timeouts. While
;; it might make sense to all threads to sleep on pollable external
;; events, we don't implement that, and it's probably simpler to
;; connect events to semaphores through a long-term poll set...

(provide sandman-add-poll-set-adder
         sandman-poll-ctx-add-poll-set-adder!
         sandman-poll-ctx-merge-timeout
         sandman-set-background-sleep!
         sandman-place-init!)

(struct exts (timeout-at fd-adders))

(define (sandman-add-poll-set-adder old-exts adder)
  (exts (and old-exts (exts-timeout-at old-exts))
        (cons adder (and old-exts (exts-fd-adders old-exts)))))

(define (sandman-poll-ctx-add-poll-set-adder! poll-ctx adder)
  (define sched-info (poll-ctx-sched-info poll-ctx))
  (when sched-info
    (schedule-info-current-exts sched-info
                                (sandman-add-poll-set-adder
                                 (schedule-info-current-exts sched-info)
                                 adder))))

(define (sandman-poll-ctx-merge-timeout poll-ctx timeout)
  (define sched-info (poll-ctx-sched-info poll-ctx))
  (when sched-info
    (schedule-info-current-exts sched-info
                                ((sandman-do-merge-timeout (current-sandman))
                                 (schedule-info-current-exts sched-info)
                                 timeout))))


(define-place-local background-sleep #f)
(define-place-local background-sleep-fd #f)

(define (sandman-set-background-sleep! sleep fd)
  (set! background-sleep sleep)
  (set! background-sleep-fd fd))

(define-place-local lock (make-lock))
(define-place-local waiting-threads '())
(define-place-local awoken-threads '())

(define (sandman-place-init!)
  (set! lock (make-lock)))

(void
 (current-sandman
  (let ([timeout-sandman (current-sandman)])
    (sandman
     ;; sleep
     (lambda (exts)
       (define timeout-at (and exts (exts-timeout-at exts)))
       (define fd-adders (and exts (exts-fd-adders exts)))
       (define ps (rktio_make_poll_set rktio))
       (let loop ([fd-adders fd-adders])
         (cond
           [(not fd-adders) (void)]
           [(pair? fd-adders)
            (loop (car fd-adders))
            (loop (cdr fd-adders))]
           [else
            (fd-adders ps)]))
       (define sleep-secs (and timeout-at
                               (/ (- timeout-at (current-inexact-milliseconds)) 1000.0)))
       (unless (and sleep-secs (sleep-secs . <= . 0.0))
         (cond
           [background-sleep
            (rktio_start_sleep rktio (or sleep-secs 0.0) ps rktio_NULL background-sleep-fd)
            (background-sleep)
            (rktio_end_sleep rktio)]
           [else
            (rktio_sleep rktio
                         (or sleep-secs 0.0)
                         ps
                         rktio_NULL)]))
       (rktio_poll_set_forget rktio ps))
     
     ;; poll
     (lambda (mode wakeup)
       (let check-signals ()
         (define v (rktio_poll_os_signal rktio))
         (unless (eqv? v RKTIO_OS_SIGNAL_NONE)
           ((rktio_get_ctl_c_handler) (cond
                                        [(eqv? v RKTIO_OS_SIGNAL_HUP) 'hang-up]
                                        [(eqv? v RKTIO_OS_SIGNAL_TERM) 'terminate]
                                        [else 'break]))
           (check-signals)))
       ((sandman-do-poll timeout-sandman) mode wakeup))

     ;; get-wakeup
     (lambda ()
       (rktio_get_signal_handle rktio))

     ;; wakeup
     (lambda (h)
       (rktio_signal_received_at h))

     ;; any-sleepers?
     (lambda ()
       ((sandman-do-any-sleepers? timeout-sandman)))

     ;; sleepers-external-events
     (lambda ()
       (define timeout-at ((sandman-do-sleepers-external-events timeout-sandman)))
       (and timeout-at
            (exts timeout-at #f)))

     ;; add-thread!
     (lambda (t exts)
       (define fd-adders (exts-fd-adders exts))
       (unless (or (not fd-adders)
                   (null? fd-adders))
         (internal-error "cannot sleep on fds"))
       ((sandman-do-add-thread! timeout-sandman) t (exts-timeout-at exts)))
     
     ;; remove-thread!
     (lambda (t timeout-handle)
       ((sandman-do-remove-thread! timeout-sandman) t timeout-handle))

     ;; merge-exts
     (lambda (a-exts b-exts)
       (if (and a-exts b-exts)
           (exts ((sandman-do-merge-external-event-sets
                   timeout-sandman)
                  (exts-timeout-at a-exts)
                  (exts-timeout-at b-exts))
                 (if (and (exts-fd-adders a-exts)
                          (exts-fd-adders b-exts))
                     (cons (exts-fd-adders a-exts)
                           (exts-fd-adders b-exts))
                     (or (exts-fd-adders a-exts)
                         (exts-fd-adders b-exts))))
           (or a-exts b-exts)))
     
     ;; merge-timeout
     (lambda (old-exts timeout-at)
       (exts ((sandman-do-merge-timeout timeout-sandman)
              (and old-exts
                   (exts-timeout-at old-exts))
              timeout-at)
             (and old-exts
                  (exts-fd-adders old-exts))))
     
     ;; extract-timeout
     (lambda (exts)
       (exts-timeout-at exts))

     ;; condition-wait
     (lambda (t)
       (lock-acquire lock)
       (set! waiting-threads (cons t waiting-threads))
       (lock-release lock)
       ;; awoken callback. for when thread is awoken
       (lambda ()
         (lock-acquire lock)
         (if (memq t waiting-threads)
             (begin
               (set! waiting-threads (remove t waiting-threads eq?))
               (set! awoken-threads (cons t awoken-threads))
               (rktio_signal_received_at (rktio_get_signal_handle rktio))) ;; wakeup main thread if sleeping
             (internal-error "thread is not a member of waiting-threads\n"))
         (lock-release lock)))

     ;; condition-poll
     (lambda (mode wakeup)
       (lock-acquire lock)
       (define at awoken-threads)
       (set! awoken-threads '())
       (lock-release lock)
       (for-each (lambda (t)
                   (wakeup t)) at))

     ;; any-waiters?
     (lambda ()
       (or (not (null? waiting-threads)) (not (null? awoken-threads))))
            

     ;; lock
     lock))))
