#lang racket/base
(require "host.rkt"
         "place-local.rkt"
         "custodian-object.rkt"
         "evt.rkt"
         "place-message.rkt")

(provide (struct-out place)
         make-place
         initial-place
         current-place)

(struct place (parent
               lock
               activity-canary           ; box for quick check before taking lock
               pch                       ; channel to new place
               [result #:mutable]        ; byte or #f, where #f means "not done"
               [queued-result #:mutable] ; non-#f triggers a place exit
               custodian                 ; root custodian
               [custodian-ref #:mutable] ; owning custodian
               [host-thread #:mutable]   ; host thread, needed for memory accounting
               [host-roots #:mutable]    ; continuation-independent state, needed for memory accounting
               [current-thread #:mutable] ; running Racket thread, needed for accounting
               [post-shutdown #:mutable] ; list of callbacks
               [pumpers #:mutable]       ; vector of up to three pumper threads
               [pending-break #:mutable] ; #f, 'break, 'hangup, or 'terminate
               done-waiting              ; hash table of places to ping when this one ends
               [wakeup-handle #:mutable]
               [dequeue-semas #:mutable]) ; semaphores reflecting place-channel waits to recheck
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
         #f                   ; host roots
         #f                   ; running thread
         '()                  ; post-shutdown
         #f                   ; pumper-threads
         #f                   ; pending-break
         (make-hasheq)        ; done-waiting
         #f                   ; wakeup-handle
         '()))                ; dequeue-semas

(define initial-place (make-place (host:make-mutex)
                                  root-custodian))

(define-place-local current-place initial-place)

(void (set-custodian-place! initial-place-root-custodian initial-place))
(void (set-place-host-thread! initial-place (host:get-initial-place)))
