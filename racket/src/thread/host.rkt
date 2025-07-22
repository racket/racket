#lang racket/base
(require racket/private/primitive-table
         (only-in '#%linklet primitive-table)
         (for-syntax racket/base))

(define (startup-error s)
  (raise (exn:fail (string-append "startup error: " s)
                   (current-continuation-marks))))

(void (unless (primitive-table '#%engine)
        (startup-error "engines not provided by host")))
(void (unless (primitive-table '#%pthread)
        (startup-error "pthreads not provided by host")))

(define-syntax (bounce stx)
  (syntax-case stx ()
    [(_ table bind ...)
     (with-syntax ([([orig-id here-id] ...)
                    (for/list ([bind (in-list (syntax->list #'(bind ...)))])
                      (if (identifier? bind)
                          (list bind bind)
                          bind))])
       #'(begin
           (provide here-id ...)
           (import-from-primitive-table table bind ...)))]))

;; This `#%pthread` table's entries are linked more directly
;; than `#%engine` entries:
(bounce #%pthread
        make-pthread-parameter
        unsafe-make-place-local
        unsafe-place-local-ref
        unsafe-place-local-set!
        unsafe-root-continuation-prompt-tag
        break-enabled-key
        engine-block)

(bounce #%engine
        make-engine
        make-engine-thread-cell-state
        set-engine-thread-cell-state!
        engine-timeout
        engine-return
        engine-roots
        call-with-engine-completion
        current-process-milliseconds
        set-ctl-c-handler!
        set-break-enabled-transition-hook!
        [continuation-marks host:continuation-marks]

        [poll-will-executors host:poll-will-executors]
        [make-will-executor host:make-will-executor]
        [make-late-will-executor host:make-late-will-executor]
        [will-executor? host:will-executor?]
        [will-register host:will-register]
        [will-try-execute host:will-try-execute]

        [unsafe-make-hasheq host:unsafe-make-hasheq]
        [unsafe-make-weak-hasheq host:unsafe-make-weak-hasheq]

        set-reachable-size-increments-callback!
        set-custodian-memory-use-proc!
        set-immediate-allocation-check-proc!

        ;; Just `exn:break`, etc., but the host may need
        ;; to distinguish breaks raised by the thread
        ;; implementation:
        exn:break/non-engine
        exn:break:hang-up/non-engine
        exn:break:terminate/non-engine

        ;; Check for async foreign callbacks:
        [poll-async-callbacks host:poll-async-callbacks]

        ;; Disabling interrupts prevents a race with interrupt handlers.
        ;; For example, if a GC is handled as an interrupt, then disabling
        ;; interrupts prevents a race with a GC handler, and aything that
        ;; disables interrupts can be used from a GC handler.
        [disable-interrupts host:disable-interrupts]
        [enable-interrupts host:enable-interrupts]

        ;; Support for the thread scheduler and interrupts
        ;; across places
        [sleep host:sleep]
        [get-wakeup-handle host:get-wakeup-handle]
        [wakeup host:wakeup]

        [fork-place host:fork-place]
        [place-get-inherit host:place-get-inherit]
        [start-place host:start-place]
        [exit host:exit]
        [current-place-roots host:current-place-roots]
        [get-initial-place host:get-initial-place]
        [call-with-current-continuation-roots host:call-with-current-continuation-roots]

        fork-pthread
        pthread?
        [get-thread-id get-pthread-id]
        [make-condition host:make-condition]
        [condition-wait host:condition-wait]
        [condition-signal host:condition-signal]
        [condition-broadcast host:condition-broadcast]
        [make-mutex host:make-mutex]
        [mutex-acquire host:mutex-acquire]
        [mutex-release host:mutex-release]
        threaded?

        [call-as-asynchronous-callback host:call-as-asynchronous-callback]
        [post-as-asynchronous-callback host:post-as-asynchronous-callback]

        continuation-current-primitive

        [prop:unsafe-authentic-override host:prop:unsafe-authentic-override]
        [get-system-stats host:get-system-stats]
        [internal-error host:internal-error])
