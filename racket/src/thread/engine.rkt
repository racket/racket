#lang racket/base
(require racket/private/primitive-table
         "internal-error.rkt"
         (only-in '#%linklet primitive-table)
         (for-syntax racket/base))

(void (unless (primitive-table '#%engine)
        (internal-error "engines not provided by host")))
(void (unless (primitive-table '#%pthread)
        (internal-error "pthreads not provided by host")))

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
        make-pthread-parameter)

(bounce #%engine
        make-engine
        engine-block
        engine-return
        current-engine-state
        current-process-milliseconds
        set-ctl-c-handler!
        root-continuation-prompt-tag
        break-enabled-key
        set-break-enabled-transition-hook!
        [continuation-marks host:continuation-marks]

        [poll-will-executors host:poll-will-executors]
        [make-will-executor host:make-will-executor]
        [make-stubborn-will-executor host:make-stubborn-will-executor]
        [will-executor? host:will-executor?]
        [will-register host:will-register]
        [will-try-execute host:will-try-execute]

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

        fork-pthread
        pthread?
        [get-thread-id get-pthread-id]
        [make-condition chez:make-condition]
        [condition-wait chez:condition-wait]
        [condition-signal chez:condition-signal]
        [condition-broadcast chez:condition-broadcast]
        [make-mutex chez:make-mutex]
        [mutex-acquire chez:mutex-acquire]
        [mutex-release chez:mutex-release]
        threaded?)
