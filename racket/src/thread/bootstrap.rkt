#lang racket/base
(require '#%linklet
         (only-in '#%foreign
                  make-stubborn-will-executor)
         "../common/queue.rkt")

;; Simulate engines by using the host system's threads.

;; This simulation doesn't provide a `dynamic-wind` that cooperates
;; with `break-enabled-key`, and it does not support using an
;; exception handler in an engine.

(define (make-engine thunk init-break-enabled-cell empty-config?)
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
                                      (call-with-values thunk list)))))
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

(define ctl-c-handler #f)

(define (set-ctl-c-handler! proc)
  (set! ctl-c-handler proc))

(define the-root-continuation-prompt-tag (make-continuation-prompt-tag 'root))
(define (root-continuation-prompt-tag) the-root-continuation-prompt-tag)
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
  (define x v)
  (case-lambda
    [() x]
    [(v) (set! x v)]))

(primitive-table '#%pthread
                 (hash
                  'make-pthread-parameter make-pthread-parameter))
(primitive-table '#%engine
                 (hash 
                  'make-engine make-engine
                  'engine-block engine-block
                  'engine-return (lambda args
                                   (error "engine-return: not ready"))
                  'current-process-milliseconds current-process-milliseconds
                  'set-ctl-c-handler! set-ctl-c-handler!
                  'root-continuation-prompt-tag root-continuation-prompt-tag
                  'break-enabled-key break-enabled-key
                  'set-break-enabled-transition-hook! void
                  'continuation-marks continuation-marks ; doesn't work on engines
                  'poll-will-executors poll-will-executors
                  'make-will-executor make-will-executor/notify
                  'make-stubborn-will-executor make-stubborn-will-executor/notify
                  'will-executor? will-executor/notify?
                  'will-register will-register/notify
                  'will-try-execute will-try-execute/notify
                  'exn:break/non-engine exn:break/non-engine
                  'exn:break:hang-up/non-engine exn:break:hang-up/non-engine
                  'exn:break:terminate/non-engine exn:break:terminate/non-engine
                  'poll-async-callbacks (lambda () null)
                  'disable-interrupts void
                  'enable-interrupts void
                  'fork-pthread (lambda args
                                  (error "fork-pthread: not ready"))
                  'pthread? (lambda args
                              (error "thread?: not ready"))
                  'get-thread-id (lambda args
                                   (error "get-pthread-id: not ready"))
                  'make-condition (lambda () 'condition)
                  'condition-wait (lambda args
                                    (error "condition-wait: not ready"))
                  'condition-signal (lambda args
                                      (error "condition-signal: not ready"))
                  'condition-broadcast (lambda args
                                         (error "condition-broadcast: not ready"))
                  'threaded? (lambda () #f)
                  'current-engine-state (lambda args
                                          (error "current-engine state: not ready"))
                  'make-mutex (lambda () 'mutex)
                  'mutex-acquire (lambda args
                                   (error "mutex-acquire: not ready"))
                  'mutex-release (lambda args
                                   (error "mutex-release: not ready"))))

;; add dummy definitions that implement pthreads and conditions etc.
;; dummy definitions that error
