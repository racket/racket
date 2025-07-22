#lang racket/base
(require racket/private/primitive-table
         (only-in '#%linklet primitive-table))

(provide start-atomic
         end-atomic
         start-uninterruptible
         end-uninterruptible
         atomically
         non-atomically
         atomically/no-gc-interrupts/no-wind
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
         schedule-info-did-work!
         control-state-evt
         async-evt
         schedule-info-current-exts
         current-sandman
         start-atomic/no-gc-interrupts ; => disable GC, too, if GC can call back
         end-atomic/no-gc-interrupts
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

;; Disables host interrupts, but the "no wind" part is
;; an unforced constraint: don't use anything related
;; to `dynamic-wind`, continuations, or continuation marks.
;; Cannot be exited with `non-atomically`.
(define-syntax-rule (atomically/no-gc-interrupts/no-wind e ...)
  (begin
    (start-atomic/no-gc-interrupts)
    (begin0
      (let () e ...)
      (end-atomic/no-gc-interrupts))))

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
