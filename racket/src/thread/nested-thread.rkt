#lang racket/base
(require "check.rkt"
         "atomic.rkt"
         "host.rkt"
         "thread.rkt"
         (except-in (submod "thread.rkt" scheduling)
                    thread
                    thread-dead-evt)
         "custodian.rkt"
         "semaphore.rkt")

(provide call-in-nested-thread)

(define/who (call-in-nested-thread thunk [cust (current-custodian)])
  (check who (procedure-arity-includes/c 0) thunk)
  (check who custodian? cust)
  (define init-break-cell (current-break-enabled-cell))
  (define result #f)
  (define result-kind #f)
  (define ready-sema (make-semaphore))
  (define t
    ;; Disable breaks while we set up the thread
    (with-continuation-mark
     break-enabled-key
     (make-thread-cell #f)
     (do-make-thread
      'call-in-nested-thread
      (lambda ()
        (semaphore-wait ready-sema)
        (with-handlers ([(lambda (x) #t)
                         (lambda (x)
                           (set! result-kind 'exn)
                           (set! result x))])
          (with-continuation-mark
           break-enabled-key
           init-break-cell
           (begin
             ;; Breaks can only happen here, and kills
             ;; can only happen after here
             (set! result (call-with-continuation-barrier
                           (lambda ()
                             (call-with-values (lambda ()
                                                 (call-with-continuation-prompt
                                                  thunk
                                                  (default-continuation-prompt-tag)
                                                  (lambda (thunk)
                                                    (abort-current-continuation
                                                     (default-continuation-prompt-tag)
                                                     thunk))))
                               list))))
             ;; Atomically decide that we have a value result and
             ;; terminate the thread, so that there's not a race between
             ;; detecting that the thread was killed versus deciding
             ;; that the thread completed with a value
             (atomically
              (set! result-kind 'value)
              (thread-dead! t))
             (engine-block)))))
      #:custodian cust)))
  (atomically
   (set-thread-forward-break-to! (current-thread) t))
  (semaphore-post ready-sema) ; let the nested thread run

  ;; Wait for the nested thread to complete -- and any thread nested
  ;; in that one at the time that it finished, and so on
  (define pending-break
    (let loop ([t t] [pending-break #f])
      (thread-wait t)
      (define next-pending-break (break-max pending-break (thread-pending-break t)))
      (let ([sub-t (thread-forward-break-to t)])
        (cond
          [sub-t (loop sub-t next-pending-break)]
          [else next-pending-break]))))

  ;; At this point, if `result-kind` is #f, then `t` was
  ;; killed or aborted to the original continuation
  
  (atomically
   (set-thread-forward-break-to! (current-thread) #f))

  ;; Propagate any leftover break, but give a propagated
  ;; exception priority over a break exception:
  (with-continuation-mark
   break-enabled-key
   (make-thread-cell #f)
   (begin
     
     (when pending-break
       ;; Breaks are disabled at this point, so the break won't be
       ;; signaled until `check-for-break` below
       (break-thread (current-thread) (if (eq? pending-break 'break) #f pending-break)))
     
     (when (eq? result-kind 'exn)
       (raise result))
     
     (unless (eq? result-kind 'value)
       (raise
        (exn:fail
         "call-in-nested-thread: the thread was killed, or it exited via the default error escape handler"
         (current-continuation-marks))))))
    
  (check-for-break)
  (apply values result))
