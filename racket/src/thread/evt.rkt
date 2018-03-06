#lang racket/base
(require "atomic.rkt")

(provide prop:evt
         evt?
         evt-poll

         (rename-out [the-never-evt never-evt]
                     [the-always-evt always-evt]
                     [the-async-evt async-evt])
         never-evt?
         async-evt?

         (struct-out wrap-evt)
         (struct-out handle-evt)
         (struct-out control-state-evt)
         (struct-out poll-guard-evt)
         (struct-out choice-evt)

         (struct-out poller)
         (struct-out poll-ctx)

         (struct-out delayed-poll)

         prop:secondary-evt
         poller-evt

         evt-impersonator?)

(module+ for-chaperone
  (provide primary-evt? primary-evt-ref
           secondary-evt? secondary-evt-ref
           impersonator-prop:evt))

(define-values (prop:evt primary-evt? primary-evt-ref)
  (make-struct-type-property 'evt
                             (lambda (v info)
                               (define who '|guard-for-prop:evt|)
                               (cond
                                 [(poller? v) v] ; part of the internal API, not the safe API
                                 [(evt? v) v]
                                 [(and (procedure? v)
                                       (procedure-arity-includes? v 1))
                                  v]
                                 [(exact-nonnegative-integer? v)
                                  (define init-count (cadr info))
                                  (unless (v . < . init-count)
                                    (raise-arguments-error who
                                                           "index for immutable field >= initialized-field count"
                                                           "index" v
                                                           "initialized-field count" init-count))
                                  (unless (memv v (list-ref info 5))
                                    (raise-arguments-error who "field index not declared immutable"
                                                           "field index" v))
                                  (selector-prop-evt-value
                                   (make-struct-field-accessor (list-ref info 3) v))]
                                 [else
                                  (raise-argument-error who
                                                        "(or/c evt? (procedure-arity-includes/c 1) exact-nonnegative-integer?)"
                                                        v)]))))

(struct selector-prop-evt-value (selector)
  #:authentic)

;; `prop:secondary-evt` is for primitive property types that
;; (due to histoical, bad design choices) act like `prop:evt`
;; without implying `prop:evt`. Specifically, it's used for
;; input and output ports.
(define-values (prop:secondary-evt secondary-evt? secondary-evt-ref)
  (make-struct-type-property 'secondary-evt))

(define (evt? v)
  (or (primary-evt? v)
      (secondary-evt? v)))

;; A poller as a `prop:evt` value wraps a procedure that is called
;; in atomic mode
;;   evt poll-ctx -> (values results-or-#f replacing-evt-or-#f)
;; where either a list of results is returned, indicating
;; that the event is selected, or a replacement event
;; is returned (possibly unchanged). If the replacement event
;; is a wrapper on `always-evt`, it will certainly be selected.
;; If a poller does any work that can allow some thread to
;; become unblocked, then it must tell the scheduler via
;; `schedule-info-did-work!`.
(struct poller (proc))

;; Provided to a `poller` function:
(struct poll-ctx (poll?         ; whether events are being polled once (i.e., 0 timeout)
                  select-proc   ; callback to asynchronously select the event being polled
                  sched-info    ; instructions to the scheduler, such as timeouts
                  [incomplete? #:mutable])) ; #t => getting back the same event does not imply a completed poll
;; If a `poller` callback keeps `select-proc` for asynchronous use,
;; then it should return a `control-state-evt` to ensure that
;; `select-proc` is not called if the event is abandoned.

(struct never-evt ()
  #:property prop:evt (poller (lambda (self poll-ctx)
                                (assert-atomic-mode)
                                (values #f self))))
(define the-never-evt (never-evt))

(struct always-evt ()
  #:property prop:evt (poller (lambda (self poll-ctx)
                                (assert-atomic-mode)
                                (values (list self) #f))))
(define the-always-evt (always-evt))

;; A placeholder for an event that will be selected through a callback
;; instead of polling:
(struct async-evt ()
  #:property prop:evt (poller (lambda (self poll-ctx)
                                (assert-atomic-mode)
                                (values #f self))))
(define the-async-evt (async-evt))

(struct wrap-evt (evt wrap)
  #:property prop:evt (poller (lambda (self poll-ctx)
                                (assert-atomic-mode)
                                (values #f self)))
  #:reflection-name 'evt)
(struct handle-evt wrap-evt ())

;; A `control-state-evt` enables (unsafe) cooperation with the
;; scheduler, normally produced by a `poller` callback. The `evt` is
;; typically a wrapper on `async-evt`. If the event is not selected,
;; the `interrupt-proc` plus `abandon-proc` will be called. If a
;; synchronization attempt is interrupted by a break signal, then
;; `interrupt-proc` is called, and then either `abandon-proc` or
;; `retry-proc` --- the latter when the synchronization attempt
;; continues, in which case a value might be ready immediately or the
;; event goes back to some waiting state. For example, a sempahore
;; uses `interrupt-proc` to get out of the semaphore's queue and
;; `rety-proc` gets back in line (or immediately returns if the
;; semaphore was meanwhile posted). As another example, a
;; `nack-guard-evt`'s result uses `abandon-proc` to post to the NACK
;; event.
(struct control-state-evt (evt
                           interrupt-proc ; thunk for break/kill initiated or otherwise before `abandon-proc`
                           abandon-proc ; thunk for not selected, including break/kill complete
                           retry-proc) ; thunk for resume from break; return `(values _val _ready?)`
  #:property prop:evt (poller (lambda (self poll-ctx) (values #f self))))

(struct poll-guard-evt (proc)
  #:property prop:evt (poller (lambda (self poll-ctx) (values #f self)))
  #:reflection-name 'evt)

(struct choice-evt (evts)
  #:property prop:evt (poller (lambda (self poll-ctx) (values #f self)))
  #:reflection-name 'evt)

(define-values (impersonator-prop:evt evt-impersonator? evt-impersonator-ref)
  (make-impersonator-property 'evt-impersonator))

;; Called in atomic mode
;; Checks whether an event is ready; returns the same results
;; as a poller. If getting an event requires going out of atomic mode
;; (to call a `prop:evt` procedure) then return a `delayed-poll`
;; struct.
(define (evt-poll evt poll-ctx)
  (assert-atomic-mode)
  (let* ([v (cond
              [(evt-impersonator? evt) (evt-impersonator-ref evt)]
              [(primary-evt? evt)
               (primary-evt-ref evt)]
              [else
               (secondary-evt-ref evt)])]
         [v (if (selector-prop-evt-value? v)
                ((selector-prop-evt-value-selector v) evt)
                v)])
    (cond
      [(procedure? v)
       (values #f (delayed-poll
                   ;; out of atomic mode:
                   (lambda ()
                     (let ([v (call-with-continuation-barrier (lambda () (v evt)))])
                       (cond
                         [(evt? v) v]
                         [(poller? v) (poller-evt v)]
                         [else (wrap-evt the-always-evt (lambda (v) evt))])))))]
      [(poller? v) ((poller-proc v) evt poll-ctx)]
      [(evt? v) (values #f v)]
      [else (values #f the-never-evt)])))

;; Possible result from `evt-poll`:
(struct delayed-poll (resume))

(struct poller-evt (poller)
  #:property prop:evt (struct-field-index poller))
