
;; The vector of place locals is similar to the set of virtual
;; registers, but the array can be shared by multiple Scheme threads
;; that are all in the same place.

;; The first slot in the vector holds a hash table for allocated
;; place-local values, and the rest are used by the thread, io, etc.,
;; layers for directly accessed variables.

(define NUM-PLACE-REGISTERS 64)

(define-virtual-register place-registers (make-vector NUM-PLACE-REGISTERS 0))
(define place-register-inits (make-vector NUM-PLACE-REGISTERS 0))

(define (init-place-locals!)
  (#%vector-set! (place-registers) 0 (make-weak-hasheq)))

(define-record place-local (default-v))

(define (unsafe-make-place-local v)
  (make-place-local v))

(define (unsafe-place-local-ref pl)
  (let ([v (hash-ref (#%vector-ref (place-registers) 0) pl none)])
    (if (eq? v none)
        (place-local-default-v pl)
        v)))

(define (unsafe-place-local-set! pl v)
  (hash-set! (#%vector-ref (place-registers) 0) pl v))

(define (place-local-register-ref i)
  (#%vector-ref (place-registers) i))

(define (place-local-register-set! i v)
  (#%vector-set! (place-registers) i v))

(define (place-local-register-init! i v)
  (place-local-register-set! i v)
  (#%vector-set! place-register-inits i v))

(define (get-place-registers)
  (place-registers))

(define (set-place-registers! vec)
  (place-registers vec))

;; ----------------------------------------

(define place-specific-table (make-hasheq))

(define (unsafe-get-place-table)
  place-specific-table)

;; ----------------------------------------

(define-record place (thread))

(meta-cond
 [(threaded?)
  (define (place-enabled?) #f) ;; FIXME
  (define (fork-place thunk)
    (fork-thread (lambda ()
                   (init-virtual-registers)
                   (place-registers (vector-copy place-register-inits))
                   (thunk))))]
 [else
  (define (place-enabled?) #f)
  (define (fork-place thunk) #f)])

(define start-place void)
(define (install-start-place! proc)
  (set! start-place proc))

(define (dynamic-place path sym in out err)
  (make-place
   (fork-place (lambda ()
                 (start-place path sym in out err)))))

(define (place-channel? v)
  #f)

(define-syntax define-place-not-yet-available
  (syntax-rules ()
    [(_ id)
     (define (id . args)
       (error 'id "place API not yet supported"))]
    [(_ id ...)
     (begin (define-place-not-yet-available id) ...)]))

;; This operation adds shutdown thunks to a non-main place, so it's a
;; no-op for now:
(define (unsafe-add-post-custodian-shutdown proc)
  (void))

(define-place-not-yet-available
  place-break
  place-channel-get
  place-channel-put
  place-sleep
  place-channel
  place-dead-evt
  place-kill
  place-message-allowed?
  place-wait
  place-pumper-threads
  place-shared?)
