#lang racket/base
(require "host.rkt"
         "place-local.rkt"
         "custodian-object.rkt"
         "evt.rkt"
         "place-message.rkt"
         "internal-error.rkt"
         "atomic.rkt")

(provide (struct-out place)
         make-place
         initial-place
         current-place
         increment-place-parallel-count!)

(struct place (parent
               lock                      ; lock is ordered after atomicity
               activity-canary           ; box for quick check before taking lock
               pch                       ; channel to new place
               [result #:mutable]        ; byte or #f, where #f means "not done"
               [queued-result #:mutable] ; non-#f triggers a place exit
               custodian                 ; root custodian
               [custodian-ref #:mutable] ; owning custodian
               [host-thread #:mutable]   ; host thread, needed for memory accounting
               [id #:mutable]            ; matches id of the host thread
               [host-roots #:mutable]    ; continuation-independent state, needed for memory accounting
               [current-thread #:mutable] ; running Racket thread, needed for accounting
               [post-shutdown #:mutable] ; list of callbacks
               [pumpers #:mutable]       ; vector of up to three pumper threads
               [pending-break #:mutable] ; #f, 'break, 'hang-up, or 'terminate
               done-waiting              ; hash table of places to ping when this one ends
               [wakeup-handle #:mutable]
               [dequeue-semas #:mutable] ; semaphores reflecting place-channel waits to recheck
               [future-scheduler #:mutable] ; #f or a scheduler of futures
               [schedulers #:mutable]    ; a hash table of additional future schedulers
               [active-parallel #:mutable]) ; number of parallel-thread futures running or scheduled
  #:authentic
  #:property host:prop:unsafe-authentic-override #t ; allow evt chaperone
  #:property prop:evt (struct-field-index pch)
  #:property prop:place-message (lambda (self) (lambda () (lambda () (place-pch self)))))

(define (make-place lock cust
                    #:parent [parent #f]
                    #:place-channel [pch #f])
  (place parent
         lock
         (box #f)             ; activity canary
         pch
         #f                   ; result
         #f                   ; queued-result
         cust
         #f
         #f                   ; host thread
         0                    ; id
         #f                   ; host roots
         #f                   ; running thread
         '()                  ; post-shutdown
         #f                   ; pumper-threads
         #f                   ; pending-break
         (host:unsafe-make-hasheq) ; done-waiting
         #f                   ; wakeup-handle
         '()                  ; dequeue-semas
         #f                   ; future scheduler
         (hasheq)             ; schedulers
         0))                  ; active-parallel

;; in atomic mode; returns #t if count goes to 0
(define (increment-place-parallel-count! delta)
  (define p current-place)
  (host:mutex-acquire (place-lock p))
  (define n (+ (place-active-parallel p) delta))
  (assert (n . >= . 0))
  (set-place-active-parallel! p n)
  (host:mutex-release (place-lock p))
  (eqv? n 0))

(define initial-place (make-place (host:make-mutex)
                                  root-custodian))

(define-place-local current-place initial-place)

(void (set-custodian-place! initial-place-root-custodian initial-place))
(void (set-place-host-thread! initial-place (host:get-initial-place)))
