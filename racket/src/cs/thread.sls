(library (thread)
  (export)
  (import (rename (chezpart)
                  [define chez:define])
          (rename (rumble)
                  [rumble:break-enabled-key break-enabled-key]
                  ;; Remapped to place-local register operations:
                  [unsafe-make-place-local rumble:unsafe-make-place-local]
                  [unsafe-place-local-ref rumble:unsafe-place-local-ref]
                  [unsafe-place-local-set! rumble:unsafe-place-local-set!]
                  ;; These are extracted via `#%linklet`:
                  [make-engine rumble:make-engine]
                  [engine-block rumble:engine-block]
                  [engine-timeout rumble:engine-timeout]
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
                  [fork-place rumble:fork-place]
                  [start-place rumble:start-place]
                  [fork-pthread rumble:fork-thread]
                  [threaded? rumble:threaded?]
                  [get-thread-id rumble:get-thread-id]
                  [get-initial-pthread rumble:get-initial-pthread]
                  [current-place-roots rumble:current-place-roots]
                  [set-ctl-c-handler! rumble:set-ctl-c-handler!]
                  [set-break-enabled-transition-hook! rumble:set-break-enabled-transition-hook!]
                  [set-reachable-size-increments-callback! rumble:set-reachable-size-increments-callback!]
                  [set-custodian-memory-use-proc! rumble:set-custodian-memory-use-proc!]
                  [set-immediate-allocation-check-proc! rumble:set-immediate-allocation-check-proc!]))

  (include "place-register.ss")
  (define-place-register-define place:define thread-register-start thread-register-count)
  
  ;; Special handling of `current-atomic`: use the last virtual register;
  ;; we rely on the fact that the register's default value is 0.
  (define-syntax (define stx)
    (syntax-case stx (current-atomic end-atomic-callback make-pthread-parameter unsafe-make-place-local)
      ;; Recognize definition of `current-atomic`:
      [(_ current-atomic (make-pthread-parameter 0))
       (with-syntax ([(_ id _) stx]
                     [n (datum->syntax #'here (- (virtual-register-count) 1))])
         #'(define-syntax id
             (syntax-rules ()
               [(_) (virtual-register n)]
               [(_ v) (set-virtual-register! n v)])))]
      ;; Recognize definition of `end-atomic-callback`:
      [(_ end-atomic-callback (make-pthread-parameter 0))
       (with-syntax ([(_ id _) stx]
                     [n (datum->syntax #'here (- (virtual-register-count) 2))])
         #'(define-syntax id
             (syntax-rules ()
               [(_) (virtual-register n)]
               [(_ v) (set-virtual-register! n v)])))]
      ;; Workaround for redirected access of `unsafe-make-place-local` from #%pthread:
      [(_ alias-id unsafe-make-place-local) #'(begin)]
      ;; Chain to place-register handling:
      [(_ . rest) #'(place:define . rest)]))

  ;; This implementation of `sleep`, `get-wakeup-handle`, and `wakeup` is relevant
  ;; only for running the places part of the thread demo. The relevant callbacks get
  ;; replaced by the "io" layer to use rktio-based functions.
  (define sleep-interrupted (rumble:unsafe-make-place-local #f))
  (define (sleep secs)
    (let ([isecs (inexact->exact (floor secs))]
          [zero-secs (make-time 'time-duration 0 0)]
          [pause-secs (make-time 'time-duration 100000 0)])
      (let loop ([all-secs (make-time 'time-duration
                                      (inexact->exact (floor (* (- secs isecs) 1e9)))
                                      isecs)])
        (unless (or (time<=? all-secs zero-secs)
                    (let ([b (rumble:unsafe-place-local-ref sleep-interrupted)])
                      (and b (unbox b))))
          (#%sleep pause-secs)
          (loop (subtract-duration all-secs pause-secs))))
      (let ([b (rumble:unsafe-place-local-ref sleep-interrupted)])
        (when b
          (set-box! b #f)))))
  (define (get-wakeup-handle)
    (let ([b (rumble:unsafe-place-local-ref sleep-interrupted)])
      (or b
          (begin
            ;; There's a race condition here.. Avoid triggering it
            ;; in the thread demo.
            (rumble:unsafe-place-local-set! sleep-interrupted (box #f))
            (get-wakeup-handle)))))
  (define (wakeup b)
    (set-box! b #t))

  (define (primitive-table key)
    (case key
      [(|#%pthread|)
       ;; Entries in the `#%pthread` table are referenced more
       ;; directly in "compiled/thread.scm". To make that work, the
       ;; entries need to be registered as built-in names with the
       ;; expander, and they need to be listed in
       ;; "primitives/internal.ss".
       (hasheq
        'make-pthread-parameter make-pthread-parameter
        'unsafe-root-continuation-prompt-tag unsafe-root-continuation-prompt-tag
        'break-enabled-key break-enabled-key
        ;; These are actually redirected by "place-register.ss", but
        ;; we list them here for compatibility with the bootstrapping
        ;; variant of `#%pthread`
        'unsafe-make-place-local rumble:unsafe-make-place-local
        'unsafe-place-local-ref rumble:unsafe-place-local-ref
        'unsafe-place-local-set! rumble:unsafe-place-local-set!)]
      [(|#%engine|)
       (hasheq
        'make-engine rumble:make-engine
        'engine-block rumble:engine-block
        'engine-timeout rumble:engine-timeout
        'engine-return rumble:engine-return
        'current-engine-state (lambda (v) (rumble:current-engine-state v))
        'set-ctl-c-handler! rumble:set-ctl-c-handler!
        'poll-will-executors poll-will-executors
        'make-will-executor rumble:make-will-executor
        'make-stubborn-will-executor rumble:make-stubborn-will-executor
        'will-executor? rumble:will-executor?
        'will-register rumble:will-register
        'will-try-execute rumble:will-try-execute
        'set-break-enabled-transition-hook! rumble:set-break-enabled-transition-hook!
        'continuation-marks rumble:continuation-marks
        'set-reachable-size-increments-callback! rumble:set-reachable-size-increments-callback!
        'set-custodian-memory-use-proc! rumble:set-custodian-memory-use-proc!
        'set-immediate-allocation-check-proc! rumble:set-immediate-allocation-check-proc!
        'exn:break/non-engine exn:break
        'exn:break:hang-up/non-engine exn:break:hang-up
        'exn:break:terminate/non-engine exn:break:terminate
        'current-process-milliseconds cpu-time
        'poll-async-callbacks poll-async-callbacks
        'disable-interrupts disable-interrupts
        'enable-interrupts enable-interrupts
        'sleep sleep
        'get-wakeup-handle get-wakeup-handle
        'wakeup wakeup
        'fork-place rumble:fork-place
        'start-place rumble:start-place
        'fork-pthread rumble:fork-thread
        'get-initial-place rumble:get-initial-pthread
        'current-place-roots rumble:current-place-roots
        'call-with-current-pthread-continuation call/cc
        'exit place-exit
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
                                 unsafe-semaphore-wait
                                 unsafe-semaphore-post)

  (set-scheduler-atomicity-callbacks! (lambda ()
                                        (current-atomic (fx+ (current-atomic) 1)))
                                      (lambda ()
                                        (current-atomic (fx- (current-atomic) 1))))

  (set-future-callbacks! 1/future? 1/current-future
                         future-block future-wait current-future-prompt))
