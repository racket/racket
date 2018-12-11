#lang racket/base
(require (only-in '#%linklet primitive-table)
         (only-in '#%unsafe
                  unsafe-custodian-register
                  unsafe-custodian-unregister)
         "../../thread/current-sandman.rkt"
         ffi/unsafe/atomic
         "bootstrap-rktio.rkt")

;; Approximate scheduler cooperation where `async-evt` can be used
;; within the dynamic extent of a `poller` callback to mean that the
;; poller is selected. Since `nack` propagation is based on a thread,
;; this approximation won't work right if an event is actually
;; contended. Also, `prop:secondary-evt` is just `prop:evt`, so
;; `prop:evt` cannot be mixed with `prop:input-port` or
;; `prop:output-port`.

(struct poller (proc)
  #:property prop:procedure
  (lambda (p s)
    (define async-sema (make-semaphore))
    (poll-guard-evt
     (lambda (poll?)
       (parameterize ([current-async-semaphore async-sema])
         (define-values (results new-evt)
           ((poller-proc p) (if poll? never-evt s) (poll-ctx poll? (lambda () (semaphore-post async-sema)))))
         (if results
             (wrap-evt always-evt (lambda (v) (apply values results)))
             new-evt))))))

(define (poller-evt v)
  (struct poller-evt ()
    #:property prop:evt (lambda (self) (v self)))
  (poller-evt))

(struct poll-ctx (poll? select-proc))

(define (poll-ctx-sched-info ctx) #f)

(struct control-state-evt (evt interrupt abandon retry)
  #:property prop:evt (lambda (cse)
                        (nack-guard-evt
                         (lambda (nack)
                           (thread (lambda () (sync nack) ((control-state-evt-abandon cse))))
                           (control-state-evt-evt cse)))))

(define current-async-semaphore (make-parameter #f))

(define (async-evt)
  (or (current-async-semaphore)
      (error 'async-evt "not in a `poller` callback")))

(define current-kill-callbacks (make-parameter '()))

(define (thread-push-kill-callback! p)
  (current-kill-callbacks (cons p (current-kill-callbacks))))

(define (thread-pop-kill-callback!)
  (current-kill-callbacks (cdr (current-kill-callbacks))))

(define schedule-info-current-exts
  (case-lambda
    [() #f]
    [(v) (void)]))

(define (sync-atomic-poll-evt? evt)
  (or (channel-put-evt? evt)
      (channel? evt)
      (semaphore? evt)
      (semaphore-peek-evt? evt)
      (eq? always-evt evt)
      (eq? never-evt evt)))

(define-values (prop:place-message place-message? place-message-ref)
  (make-struct-type-property 'place-message))

(define-values (prop:unsafe-authentic-override unsafe-authentic-override? unsafe-authentic-override-ref)
  (make-struct-type-property 'authentic-override))

(primitive-table '#%pthread
                 (hasheq 'unsafe-make-place-local box
                         'unsafe-place-local-ref unbox
                         'unsafe-place-local-set! set-box!
                         'unsafe-add-global-finalizer (lambda (v proc) (void))
                         'unsafe-strip-impersonator (lambda (v) v)
                         'prop:unsafe-authentic-override prop:unsafe-authentic-override))

(primitive-table '#%thread
                 (hasheq 'thread thread
                         'thread-suspend-evt thread-suspend-evt
                         'thread-dead-evt thread-dead-evt
                         'current-thread current-thread
                         'thread-resume thread-resume
                         'make-semaphore make-semaphore
                         'semaphore-post semaphore-post
                         'semaphore-wait semaphore-wait
                         'semaphore-peek-evt semaphore-peek-evt
                         'make-channel make-channel
                         'channel-put-evt channel-put-evt
                         'wrap-evt wrap-evt
                         'handle-evt handle-evt
                         'always-evt always-evt
                         'choice-evt (lambda (l) (apply choice-evt l))
                         'sync sync
                         'sync/timeout sync/timeout
                         'sync-atomic-poll-evt? sync-atomic-poll-evt?
                         'evt? evt?
                         'prop:evt prop:evt
                         'prop:secondary-evt prop:evt
                         'poller poller
                         'poller-evt poller-evt
                         'poll-ctx-poll? poll-ctx-poll?
                         'poll-ctx-select-proc poll-ctx-select-proc
                         'poll-ctx-sched-info poll-ctx-sched-info
                         'set-poll-ctx-incomplete?! void
                         'schedule-info-did-work! void
                         'control-state-evt control-state-evt
                         'async-evt async-evt
                         'schedule-info-current-exts schedule-info-current-exts
                         'current-sandman current-sandman
                         'unsafe-start-atomic start-atomic
                         'unsafe-end-atomic end-atomic
                         'start-atomic/no-interrupts start-atomic
                         'end-atomic/no-interrupts end-atomic
                         'in-atomic-mode? in-atomic-mode?
                         'current-custodian current-custodian
                         'custodian-shut-down? (lambda (c)
                                                 (define v (box 1))
                                                 (define ref (unsafe-custodian-register c v void #f #f))
                                                 (cond
                                                   [ref (unsafe-custodian-unregister v ref) #f]
                                                   [else #t]))
                         'current-plumber current-plumber
                         'plumber-add-flush! plumber-add-flush!
                         'plumber-flush-handle-remove! plumber-flush-handle-remove!
                         'unsafe-custodian-register unsafe-custodian-register
                         'unsafe-custodian-unregister unsafe-custodian-unregister
                         'thread-push-kill-callback! thread-push-kill-callback!
                         'thread-pop-kill-callback! thread-pop-kill-callback!
                         'unsafe-add-pre-poll-callback! (lambda (proc) (void))
                         'set-get-subprocesses-time! void
                         'prop:place-message prop:place-message))

(primitive-table '#%windows-version
                 (hasheq 'get-windows-version (lambda ()
                                                (values 'major
                                                        'minor
                                                        'build-number
                                                        #"CSDVersion (possibly empty)"))))
