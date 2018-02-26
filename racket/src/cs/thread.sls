(library (thread)
  (export)
  (import (rename (chezpart)
                  [define chez:define])
          (rename (only (chezscheme)
                        sleep
                        printf)
                  [sleep chez:sleep])
          (rename (rumble)
                  [rumble:break-enabled-key break-enabled-key]
                  ;; These are extracted via `#%linklet`:
                  [make-engine rumble:make-engine]
                  [engine-block rumble:engine-block]
                  [engine-return rumble:engine-return]
                  [current-engine-state rumble:current-engine-state]
                  [make-condition rumble:make-condition]
                  [condition-wait rumble:condition-wait]
                  [condition-signal rumble:condition-signal]
                  [condition-broadcast rumble:condition-broadcast]
                  [make-mutex rumble:make-mutex]
                  [mutex-acquire rumble:mutex-acquire]
                  [mutex-release rumble:mutex-release]
                  [pthread? rumble:thread?]
                  [fork-pthread rumble:fork-thread]
                  [threaded? rumble:threaded?]
                  [get-thread-id rumble:get-thread-id]
                  [set-ctl-c-handler! rumble:set-ctl-c-handler!]
                  [root-continuation-prompt-tag rumble:root-continuation-prompt-tag]
                  [set-break-enabled-transition-hook! rumble:set-break-enabled-transition-hook!]))

  ;; Special handling of `current-atomic`: use the last virtual register.
  ;; We rely on the fact that the register's default value is 0.
  (define-syntax (define stx)
    (syntax-case stx (current-atomic make-pthread-parameter)
      [(_ current-atomic (make-pthread-parameter 0))
       (with-syntax ([(_ id _) stx]
                     [n (datum->syntax #'here (sub1 (virtual-register-count)))])
         #'(define-syntax id
             (syntax-rules ()
               [(_) (virtual-register n)]
               [(_ v) (set-virtual-register! n v)])))]
      [(_ . rest) #'(chez:define . rest)]))

  (define (exit n)
    (chez:exit n))

  (define (sleep secs)
    (define isecs (inexact->exact (floor secs)))
    (chez:sleep (make-time 'time-duration
                           (inexact->exact (floor (* (- secs isecs) 1e9)))
                           isecs)))

  (define (primitive-table key)
    (case key
      [(|#%pthread|)
       ;; Entries in the `#%pthread` table are referenced more
       ;; directly in "compiled/thread.scm". To make that work, the
       ;; entries need to be registered as built-in names with the
       ;; expander, and they need to be listed in
       ;; "primitives/internal.ss".
       (hash
        'make-pthread-parameter make-pthread-parameter)]
      [(|#%engine|)
       (hash
        'make-engine rumble:make-engine
        'engine-block rumble:engine-block
        'engine-return rumble:engine-return
        'current-engine-state (lambda (v) (rumble:current-engine-state v))
        'set-ctl-c-handler! rumble:set-ctl-c-handler!
        'root-continuation-prompt-tag rumble:root-continuation-prompt-tag
        'poll-will-executors poll-will-executors
        'make-will-executor rumble:make-will-executor
        'make-stubborn-will-executor rumble:make-stubborn-will-executor
        'will-executor? rumble:will-executor?
        'will-register rumble:will-register
        'will-try-execute rumble:will-try-execute
        'break-enabled-key break-enabled-key
        'set-break-enabled-transition-hook! rumble:set-break-enabled-transition-hook!
        'continuation-marks rumble:continuation-marks
        'exn:break/non-engine exn:break
        'exn:break:hang-up/non-engine exn:break:hang-up
        'exn:break:terminate/non-engine exn:break:terminate
        'current-process-milliseconds cpu-time
        'poll-async-callbacks poll-async-callbacks
        'disable-interrupts disable-interrupts
        'enable-interrupts enable-interrupts
        'fork-pthread rumble:fork-thread
        'pthread? rumble:thread?
        'get-thread-id rumble:get-thread-id
        'make-condition rumble:make-condition
        'condition-wait rumble:condition-wait
        'condition-signal rumble:condition-signal
        'condition-broadcast rumble:condition-broadcast
        'make-mutex rumble:make-mutex
        'mutex-acquire rumble:mutex-acquire
        'mutex-release rumble:mutex-release
        'threaded? rumble:threaded?)]
      [else #f]))

  ;; Tie knots:
  (define (check-for-break) (1/check-for-break))
  (define (break-enabled) (1/break-enabled))

  (include "include.ss")
  (include-generated "thread.scm")

  (set-engine-exit-handler!
   (lambda (v)
     (|#%app| (|#%app| 1/exit-handler) v)))

  (set-scheduler-lock-callbacks! (lambda () (1/make-semaphore 1))
                                 1/semaphore-wait
                                 1/semaphore-post)

  (set-scheduler-atomicity-callbacks! (lambda ()
                                        (current-atomic (fx+ (current-atomic) 1)))
                                      (lambda ()
                                        (current-atomic (fx- (current-atomic) 1))))

  (set-future-callbacks! 1/future? 1/current-future
                         future-block future-wait current-future-prompt))
