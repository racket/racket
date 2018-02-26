#lang racket/base
(require "check.rkt"
         (rename-in "semaphore.rkt"
                    [semaphore-peek-evt raw:semaphore-peek-evt])
         (rename-in "evt.rkt"
                    [wrap-evt raw:wrap-evt]
                    [handle-evt raw:handle-evt]
                    [handle-evt? raw:handle-evt?]
                    [poll-guard-evt raw:poll-guard-evt]
                    [choice-evt raw:choice-evt])
         (only-in "sync.rkt"
                  sync/enable-break))

(provide wrap-evt
         handle-evt
         handle-evt?
         guard-evt
         poll-guard-evt
         nack-guard-evt
         choice-evt
         semaphore-peek-evt
         semaphore-wait/enable-break
         call-with-semaphore
         call-with-semaphore/enable-break)

(define/who (choice-evt . args)
  (for ([arg (in-list args)])
    (check who evt? arg))
  (raw:choice-evt args))

(define/who (wrap-evt evt proc)
  (check who evt? evt)
  (check who procedure? proc)
  (raw:wrap-evt evt proc))

(define/who (handle-evt evt proc)
  (check who evt? evt)
  (check who procedure? proc)
  (raw:handle-evt evt proc))

(define/who (handle-evt? evt)
  (check who evt? evt)
  (let loop ([evt evt])
    (or (raw:handle-evt? evt)
        (and (choice-evt? evt)
             (for/or ([evt (in-list (choice-evt-evts evt))])
               (loop evt))))))

(define/who (guard-evt proc)
  (check who (procedure-arity-includes/c 0) proc)
  (raw:poll-guard-evt (lambda (poll?) (proc))))

(define/who (poll-guard-evt proc)
  (check who(procedure-arity-includes/c 1) proc)
  (raw:poll-guard-evt proc))

(define/who (nack-guard-evt proc)
  (check who (procedure-arity-includes/c 1) proc)
  (raw:poll-guard-evt
   (lambda (poll?)
     (define s (make-semaphore))
     ;; Return control-state-evt to register
     ;; the nack semaphore before exposing it to
     ;; the `proc` callback:
     (control-state-evt
      (raw:poll-guard-evt
       (lambda (poll?)
         (define v (proc (wrap-evt (raw:semaphore-peek-evt s) void)))
         (if (evt? v)
             v
             (wrap-evt always-evt (lambda () v)))))
      void
      (lambda () (semaphore-post s))
      void))))

(define/who (semaphore-peek-evt s)
  (check who semaphore? s)
  (raw:semaphore-peek-evt s))

(define/who (semaphore-wait/enable-break s)
  (check who semaphore? s)
  (sync/enable-break s)
  (void))

;; ----------------------------------------

(define (do-call-with-semaphore who s proc try-fail args #:enable-break? [enable-break? #f])
  (check who semaphore? s)
  (check who procedure? proc)
  (check who (procedure-arity-includes/c 0) #:or-false try-fail)
  (define breaks-on? (or enable-break?
                         (break-enabled)))
  (define results #t) ; transitions to list of results unless semaphore-try fails
  (dynamic-wind
   (lambda ()
     (if try-fail
         (set! results (semaphore-try-wait? s))
         (if breaks-on?
             (semaphore-wait/enable-break s)
             (semaphore-wait s))))             
   (lambda ()
     (when results
       (call-with-continuation-barrier
        (lambda ()
          (set! results
                (call-with-values (lambda () (apply proc args)) list))))))
   (lambda ()
     (when results
       (semaphore-post s))))
  (if results
      (apply values results)
      (try-fail)))

(define (call-with-semaphore s proc [try-fail #f] . args)
  (do-call-with-semaphore 'call-with-semaphore s proc try-fail args))

(define (call-with-semaphore/enable-break s proc [try-fail #f] . args)
  (do-call-with-semaphore 'call-with-semaphore/enable-break s proc try-fail args #:enable-break? #t))
