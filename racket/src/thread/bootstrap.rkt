#lang racket/base
(require '#%linklet
         (only-in '#%foreign
                  make-stubborn-will-executor)
         "../common/queue.rkt")

;; Simulate engines by using the host system's threads.

;; This simulation doesn't provide a `dynamic-wind` that cooperates
;; with `break-enabled-key`, and it does not support using an
;; exception handler in an engine.

(provide register-place-symbol!
         set-io-place-init!)

(define (make-engine thunk prompt-tag init-break-enabled-cell empty-config?)
  (define ready-s (make-semaphore))
  (define s (make-semaphore))
  (define prefix void)
  (define results (list (void)))
  (define t (thread (lambda ()
                      (define orig (uncaught-exception-handler))
                      (define (run-prefix)
                        (prefix)
                        (set! prefix void))
                      (call-with-exception-handler
                       (lambda (exn)
                         (if (and (exn:break? exn)
                                  (not (exn:break/non-engine? exn)))
                             (with-handlers ([exn:break/non-engine?
                                              (lambda (exn)
                                                ;; Avoid exception-during-exception
                                                ;; error by propagating the original,
                                                ;; even though it's a different kind
                                                ;; of break exn:
                                                exn)])
                               (run-prefix)
                               ((exn:break-continuation exn)))
                             (abort-current-continuation
                              the-root-continuation-prompt-tag
                              exn)))
                       (lambda ()
                         (call-with-continuation-prompt
                          (lambda ()
                            (with-continuation-mark
                                break-enabled-key
                                init-break-enabled-cell
                              (begin
                                (semaphore-post ready-s)
                                (semaphore-wait s)
                                (run-prefix)
                                (set! results
                                      (call-with-continuation-prompt
                                       (lambda ()
                                         (call-with-values thunk list))
                                       prompt-tag
                                       (lambda (proc)
                                         (abort-current-continuation prompt-tag proc)))))))
                          the-root-continuation-prompt-tag
                          (lambda (exn)
                            ((error-display-handler) (exn-message exn) exn))))))))
  (semaphore-wait ready-s)
  (thread-suspend t)
  (semaphore-post s)
  (define (go ticks next-prefix complete expire)
    (set! prefix next-prefix)
    (break-thread t)
    (thread-resume t)
    (define t2
      (thread (lambda ()
                (sleep (/ ticks 1000000.0))
                (thread-suspend t))))
    ;; Limited break propagation while syncing:
    (call-with-exception-handler
     (lambda (exn)
       (if (and (exn:break? exn)
                ctl-c-handler)
           (begin
             (ctl-c-handler 'break)
             ((exn:break-continuation exn)))
           exn))
     (lambda ()
       (sync t t2 (thread-suspend-evt t))))
    (cond
     [(thread-dead? t)
      (apply complete 0 results)]
     [else
      (expire go)]))
  go)

(define (engine-block)
  (thread-suspend (current-thread)))

(define (engine-timeout)
  (void))

(define ctl-c-handler #f)

(define (set-ctl-c-handler! proc)
  (set! ctl-c-handler proc))

(define the-root-continuation-prompt-tag (make-continuation-prompt-tag 'root))
(define (unsafe-root-continuation-prompt-tag) the-root-continuation-prompt-tag)
(define break-enabled-key (gensym 'break-enabled))

(struct will-executor/notify (we queue notify))

(define will-executors null)

(define (poll-will-executors)
  (when (for/or ([w (in-list will-executors)])
          (will-try-execute w))
    (poll-will-executors)))

(define (do-make-will-executor/notify make-will-executor notify)
  (define we (make-will-executor))
  (set! will-executors (cons we will-executors))
  (will-executor/notify we (make-queue) notify))

(define (make-will-executor/notify notify)
  (do-make-will-executor/notify make-will-executor notify))

(define (make-stubborn-will-executor/notify notify)
  (do-make-will-executor/notify make-stubborn-will-executor notify))

(define (will-register/notify we/n v proc)
  (will-register (will-executor/notify-we we/n)
                 v
                 (lambda (v)
                   ((will-executor/notify-notify we/n))
                   (queue-add! (will-executor/notify-queue we/n)
                               (cons proc v)))))

(define (will-try-execute/notify we/n)
  (poll-will-executors)
  (queue-remove! (will-executor/notify-queue we/n)))

(define (will-executor-notification-procedure we [proc #f])
  (error "will-executor-notification-procedure not supported"))

(struct exn:break/non-engine exn:break ())
(struct exn:break:hang-up/non-engine exn:break/non-engine ())
(struct exn:break:terminate/non-engine exn:break/non-engine ())

(define (make-pthread-parameter v)
  (define l (unsafe-make-place-local v))
  (case-lambda
    [() (unsafe-place-local-ref l)]
    [(v) (unsafe-place-local-set! l v)]))

(define initial-place-local-table (make-hasheq))
(define place-local-table (make-parameter initial-place-local-table))

(define (unsafe-make-place-local v)
  (define key (vector v 'place-locale))
  (hash-set! (place-local-table) key v)
  key)

(define (unsafe-place-local-ref key)
  (hash-ref (place-local-table) key (vector-ref key 0)))

(define (unsafe-place-local-set! key val)
  (hash-set! (place-local-table) key val))

(define wakeables (make-weak-hasheq))

(define (wakeable-sleep msecs)
  (define s (make-semaphore))
  (hash-set! wakeables (place-local-table) s)
  (sync/timeout msecs s)
  (void))

(define (get-wakeup-handle)
  (place-local-table))

(define (wakeup t)
  (define s (hash-ref wakeables t #f))
  (when s (semaphore-post s)))

(define place-done-prompt (make-continuation-prompt-tag 'place-done))

;; Beware that this implementation of `fork-place` doesn't support
;; rktio-based blocking in different places. So, be careful of the
;; preliminary tests that you might try with the "io" layer and
;; places.
(define (fork-place thunk finish)
  (parameterize ([place-local-table (make-hasheq)])
    (thread (lambda ()
              (define v
                (call-with-continuation-prompt
                 thunk
                 place-done-prompt))
              (finish v)))))               

(define place-symbols (make-hasheq))
(define io-place-init! void)

(define (start-place pch mod sym in-fd out-fd err-fd cust plumber)
  (io-place-init! in-fd out-fd err-fd cust plumber)
  (lambda ()
    ((hash-ref place-symbols sym) pch)))

;; For use in "demo.rkt"
(define (register-place-symbol! sym proc)
  (hash-set! place-symbols sym proc))

;; For use in "demo-thread.rkt" in "io"
(define (set-io-place-init! proc)
  (set! io-place-init! proc))

(define (place-exit v)
  (if (eq? initial-place-local-table (place-local-table))
      (exit v)
      (abort-current-continuation
       place-done-prompt
       (lambda () v))))

(primitive-table '#%pthread
                 (hash
                  'make-pthread-parameter make-pthread-parameter
                  'unsafe-make-place-local unsafe-make-place-local
                  'unsafe-place-local-ref unsafe-place-local-ref
                  'unsafe-place-local-set! unsafe-place-local-set!
                  'unsafe-add-global-finalizer (lambda (v proc) (void))
                  'unsafe-root-continuation-prompt-tag unsafe-root-continuation-prompt-tag
                  'break-enabled-key break-enabled-key))
(primitive-table '#%engine
                 (hash 
                  'make-engine make-engine
                  'engine-block engine-block
                  'engine-timeout engine-timeout
                  'engine-return (lambda args
                                   (error "engine-return: not ready"))
                  'current-process-milliseconds current-process-milliseconds
                  'set-ctl-c-handler! set-ctl-c-handler!
                  'set-break-enabled-transition-hook! void
                  'continuation-marks continuation-marks ; doesn't work on engines
                  'poll-will-executors poll-will-executors
                  'make-will-executor make-will-executor/notify
                  'make-stubborn-will-executor make-stubborn-will-executor/notify
                  'will-executor? will-executor/notify?
                  'will-register will-register/notify
                  'will-try-execute will-try-execute/notify
                  'set-reachable-size-increments-callback! (lambda (proc) (void))
                  'set-custodian-memory-use-proc! (lambda (proc) (void))
                  'set-immediate-allocation-check-proc! (lambda (proc) (void))
                  'exn:break/non-engine exn:break/non-engine
                  'exn:break:hang-up/non-engine exn:break:hang-up/non-engine
                  'exn:break:terminate/non-engine exn:break:terminate/non-engine
                  'poll-async-callbacks (lambda () null)
                  'disable-interrupts void
                  'enable-interrupts void
                  'sleep wakeable-sleep
                  'get-wakeup-handle get-wakeup-handle
                  'wakeup wakeup
                  'fork-place fork-place
                  'start-place start-place
                  'exit place-exit
                  'fork-pthread (lambda args
                                  (error "fork-pthread: not ready"))
                  'pthread? (lambda args
                              (error "thread?: not ready"))
                  'get-thread-id (lambda () 0)
                  'current-place-roots (lambda () '())
                  'get-initial-place (lambda () #f)
                  'call-with-current-place-continuation call/cc
                  'make-condition (lambda () (make-semaphore))
                  'condition-wait (lambda (c s)
                                    (semaphore-post s)
                                    (semaphore-wait c)
                                    (semaphore-wait s))
                  'condition-signal (lambda (c)
                                      (semaphore-post c))
                  'condition-broadcast (lambda args
                                         (error "condition-broadcast: not ready"))
                  'threaded? (lambda () #f)
                  'current-engine-state (lambda args
                                          (error "current-engine state: not ready"))
                  'make-mutex (lambda () (make-semaphore 1))
                  'mutex-acquire (lambda (s) (semaphore-wait s))
                  'mutex-release (lambda (s) (semaphore-post s))))

;; add dummy definitions that implement pthreads and conditions etc.
;; dummy definitions that error
