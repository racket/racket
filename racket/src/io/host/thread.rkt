#lang racket/base
(require racket/private/primitive-table
         (only-in '#%linklet primitive-table))

(provide start-atomic
         end-atomic
         start-uninterruptible
         end-uninterruptible
         atomically
         non-atomically
         assert-atomic
         check-current-custodian)

(define table
  (or (primitive-table '#%thread)
      (error '#%thread "scheduler cooperation not supported by host")))

(define-syntax-rule (bounce id ...)
  (begin
    (provide id ...)
    (import-from-primitive-table
     (#%thread)
     id ...)))

;; Values with `bounce*` cannot be redirected
;; to refer directly to exports of `thread`,
;; generally because there's no such export
(define-syntax-rule (bounce* id ...)
  (begin
    (provide id ...)
    (define id (hash-ref table 'id))
    ...))

(bounce thread
        thread-suspend-evt
        thread-dead-evt
        current-thread
        thread-resume
        make-semaphore
        semaphore-post
        semaphore-post-all
        semaphore-wait
        semaphore-peek-evt
        make-channel
        channel-put-evt
        wrap-evt
        handle-evt
        always-evt
        sync
        sync/timeout
        evt?
        prop:evt
        unsafe-start-atomic
        unsafe-end-atomic
        unsafe-start-uninterruptible
        unsafe-end-uninterruptible
        unsafe-make-uninterruptible-lock
        unsafe-uninterruptible-lock-acquire
        unsafe-uninterruptible-lock-release
        current-custodian
        custodian-shut-down?
        current-plumber
        plumber-add-flush!
        plumber-flush-handle-remove!
        prop:place-message)

(bounce* choice-evt ; raw variant that takes a list of evts
         prop:secondary-evt
         sync-atomic-poll-evt?
         poller
         poller-evt
         poll-ctx-poll?
         poll-ctx-select-proc
         poll-ctx-sched-info
         set-poll-ctx-incomplete?!
         delayed-poll
         channel-get-poll-or-semaphore
         channel-put-poll-or-semaphore
         schedule-info-did-work!
         control-state-evt
         async-evt
         schedule-info-current-exts
         current-sandman
         start-atomic/no-gc-interrupts ; => disable GC, too, if GC can call back
         end-atomic/no-gc-interrupts
         start-uninterruptible/no-gc-interrupts ; => disable GC, too, if GC can call back
         end-uninterruptible/no-gc-interrupts
         in-atomic-mode?
         unsafe-custodian-register
         unsafe-custodian-unregister
         unsafe-make-custodian-at-root
         thread-push-kill-callback!
         thread-pop-kill-callback!
         unsafe-add-pre-poll-callback!
         set-get-subprocesses-time!)

(define start-atomic unsafe-start-atomic)
(define end-atomic unsafe-end-atomic)
(define start-uninterruptible unsafe-start-uninterruptible)
(define end-uninterruptible unsafe-end-uninterruptible)

(define-syntax-rule (atomically e ...)
  (begin
    (start-atomic)
    (begin0
      (let () e ...)
      (end-atomic))))

(define-syntax-rule (non-atomically e ...)
  (begin
    (end-atomic)
    (begin0
      (let () e ...)
      (start-atomic))))

;; Enable for debugging
(define (assert-atomic)
  (void)
  #;
  (unless (in-atomic-mode?)
    (error 'assert-atomic "not in atomic mode")))

;; in atomic mode
(define (check-current-custodian who #:unlock [unlock end-atomic])
  (when (custodian-shut-down? (current-custodian))
    (unlock)
    (raise
     (exn:fail
      (string-append (symbol->string who) ": the current custodian has been shut down")
      (current-continuation-marks)))))
