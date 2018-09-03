#lang racket/base
(require (only-in '#%unsafe unsafe-abort-current-continuation/no-wind)
         "place-local.rkt"
         "check.rkt"
         "host.rkt"
         "schedule.rkt"
         "atomic.rkt"
         "thread.rkt"
         (submod "thread.rkt" for-place)
         "custodian.rkt"
         (submod "custodian.rkt" scheduling)
         "plumber.rkt"
         "exit.rkt"
         "sync.rkt"
         "evt.rkt"
         "sandman.rkt")

(provide dynamic-place
         place?
         place-break
         place-kill
         place-wait
         place-dead-evt
         place-sleep

         place-channel
         place-channel? 
         place-channel-get
         place-channel-put
         place-message-allowed?

         place-pumper-threads
         place-shared?
         unsafe-add-post-custodian-shutdown)

;; ----------------------------------------

(struct place (lock
               [result #:mutable]        ; byte or #f, where #f means "not done"
               [queued-result #:mutable] ; non-#f triggers a place exit
               custodian
               [post-shutdown #:mutable] ; list of callbacks
               pumper-threads            ; vector of up to three pumper threads
               [pending-break #:mutable] ; #f, 'break, 'hangup, or 'terminate
               done-waiting              ; hash table of places to ping when this one ends
               [wakeup-handle #:mutable]))

(define-place-local current-place #f)

(define (dynamic-place path sym in out err)
  (define c (create-custodian))
  (define lock (host:make-mutex))
  (define started (host:make-condition))
  (define done-waiting (make-hasheq))
  (define new-place (place lock
                           #f                   ; result
                           #f                   ; queued-result
                           c
                           '()                  ; post-shutdown
                           (make-vector 3 #f)   ; pumper-threads
                           #f                   ; pending-break
                           done-waiting
                           #f))
  (define orig-plumber (make-plumber))
  (define (default-exit v)
    (plumber-flush-all orig-plumber)
    (host:mutex-acquire lock)
    (set-place-queued-result! new-place (if (byte? v) v 0))
    (host:mutex-release lock)
    ;; Switch to scheduler, so it can exit:
    (engine-block))
  ;; Start the new place
  (host:fork-place
   (lambda ()
     (define finish (host:start-place path sym in out err))
     (call-in-another-main-thread
      c
      (lambda ()
        (set! current-place new-place)
        (current-plumber orig-plumber)
        (exit-handler default-exit)
        ;; The finish function reports some I/O related
        ;; information to store in the place, and when that
        ;; callback returns, it starts loading the specified
        ;; module
        (call-with-continuation-prompt
         (lambda ()
           (finish
            (lambda (in-th out-th err-th)
              (vector-set! (place-pumper-threads new-place) 0 in-th)
              (vector-set! (place-pumper-threads new-place) 1 out-th)
              (vector-set! (place-pumper-threads new-place) 2 err-th)
              (host:mutex-acquire lock)
              (set-place-wakeup-handle! new-place (sandman-get-wakeup-handle))
              (host:condition-signal started) ; place is sufficiently started
              (host:mutex-release lock))))
         (default-continuation-prompt-tag)
         (lambda (thunk)
           ;; Thread ended with escape => exit with status 1
           (call-with-continuation-prompt thunk)
           (default-exit 1)))
        (default-exit 0))))
   (lambda (result)
     ;; Place is done, so save the result and alert anyone waiting on
     ;; the place
     (do-custodian-shutdown-all c)
     (host:mutex-acquire lock)
     (set-place-result! new-place result)
     (host:mutex-release lock)
     (for ([k (in-hash-keys done-waiting)])
       (cond
         [(place? k)
          (host:mutex-acquire (place-lock k))
          (unless (place-result k)
            (sandman-wakeup (place-wakeup-handle k)))
          (host:mutex-release (place-lock k))]
         [else (sandman-wakeup k)]))
     (hash-clear! done-waiting)))
  ;; Wait for the place to start, then return the place object
  (host:mutex-acquire lock)
  (host:condition-wait started lock)
  (host:mutex-release lock)
  new-place)

(define/who (place-break p [kind #f])
  (check who place? p)
  (unless (or (not kind) (eq? kind 'hangup) (eq? kind 'terminate))
    (raise-argument-error who "(or/c #f 'hangup 'terminate)" kind))
  (host:mutex-acquire (place-lock p))
  (define pending-break (place-pending-break p))
  (when (or (not pending-break)
            (break>? (or kind 'break) pending-break))
    (set-place-pending-break! p (or kind 'break))
    (sandman-wakeup (place-wakeup-handle p)))
  (host:mutex-release (place-lock p)))

(void
 (set-check-place-break!
  ;; Called in atomic mode by scheduler
  (lambda ()
    (define p current-place)
    (when p
      (host:mutex-acquire (place-lock p))
      (define queued-result (place-queued-result p))
      (define break (place-pending-break p))
      (when break
        (set-place-pending-break! p #f))
      (host:mutex-release (place-lock p))
      (when queued-result
        (force-exit queued-result))
      (when break
        (do-break-thread root-thread break #f))))))

(define/who (place-kill p)
  (check who place? p)
  (host:mutex-acquire (place-lock p))
  (unless (or (place-result p)
              (place-queued-result p))
    (set-place-queued-result! p 1))
  (host:mutex-release (place-lock p))
  (place-wait p)
  (void))

(define/who (place-wait p)
  (check who place? p)
  (sync (place-done-evt p #t)))

(struct place-done-evt (p get-result?)
  #:property prop:evt (poller (lambda (self poll-ctx)
                                (assert-atomic-mode)
                                (define p (place-done-evt-p self))
                                (host:mutex-acquire (place-lock p))
                                (define result (place-result p))
                                (unless result
                                  (hash-set! (place-done-waiting p)
                                             (or current-place
                                                 (sandman-get-wakeup-handle))
                                             #t))
                                (host:mutex-release (place-lock p))
                                (if result
                                    (if (place-done-evt-get-result? self)
                                        (values (list result) #f)
                                        (values (list self) #f))
                                    (values #f self))))
  #:reflection-name 'place-dead-evt)

(define/who (place-dead-evt p)
  (check who place? p)
  (place-done-evt p #f))

(define/who (place-sleep msecs)
  (void))

;; ----------------------------------------

(struct pchannel ()
  #:reflection-name 'place-channel)

(define (place-channel? v)
  (pchannel? v))

(define (place-channel)
  (values (pchannel)
          (pchannel)))

(define (place-channel-get pch)
  (sync never-evt))

(define (place-channel-put pch v)
  (void))

(define (place-message-allowed? v)
  #t)

;; ----------------------------------------

(define (place-shared? v)
  #f)

(define (unsafe-add-post-custodian-shutdown proc)
  (when current-place
    (atomically
     (set-place-post-shutdown! current-place
                               (cons proc
                                     (place-post-shutdown current-place))))))
