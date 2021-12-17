
;; The vector of place locals is similar to the set of virtual
;; registers, but the array can be shared by multiple Scheme threads
;; that are all in the same place.

;; The first slot in the vector holds a hash table for allocated
;; place-local values, the last is used by "async-callback.ss", and
;; the rest are used by the thread, io, etc., layers for directly
;; accessed variables.

(define NUM-PLACE-REGISTERS 128) ; 3 through 126 available for subsystems

(define LOCAL_TABLE-INDEX 0)
(define ASYNC-CALLBACK-REGISTER-INDEX 1)
;; index 2 is available

(define-virtual-register place-registers (#%make-vector NUM-PLACE-REGISTERS 0))
(define place-register-inits (#%make-vector NUM-PLACE-REGISTERS 0))

(define (init-place-locals!)
  (#%vector-set! (place-registers) LOCAL_TABLE-INDEX (make-weak-hasheq)))

(define-record place-local (default-v))

(define (unsafe-make-place-local v)
  (make-place-local v))

(define (unsafe-place-local-ref pl)
  (let ([v (hash-ref (#%vector-ref (place-registers) LOCAL_TABLE-INDEX) pl none)])
    (if (eq? v none)
        (place-local-default-v pl)
        v)))

(define (unsafe-place-local-set! pl v)
  (hash-set! (#%vector-ref (place-registers) LOCAL_TABLE-INDEX) pl v))

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

(define place-async-callback-queue
  (case-lambda
   [() (let ([v (#%vector-ref (place-registers) ASYNC-CALLBACK-REGISTER-INDEX)])
         (if (eqv? v 0)
             #f
             v))]
   [(v) (#%vector-set! (place-registers) ASYNC-CALLBACK-REGISTER-INDEX v)]))

;; ----------------------------------------

(define place-specific-table (unsafe-make-place-local #f))

(define (unsafe-get-place-table)
  (with-interrupts-disabled
   (or (unsafe-place-local-ref place-specific-table)
       (let ([ht (make-hasheq)])
         (unsafe-place-local-set! place-specific-table ht)
         ht))))

;; ----------------------------------------

(define-thread-local place-esc-box (box #f))

(meta-cond
 [(threaded?)
  (define (place-enabled?) #t)
  (define (fork-place thunk finish-proc)
    (do-prepare-for-place)
    (fork-thread (lambda ()
                   (collect-trip-for-allocating-places! +1)
                   (thread-preserve-ownership!) ; encourages parallel GC
                   (init-virtual-registers)
                   (place-registers (vector-copy place-register-inits))
                   (root-thread-cell-values (make-empty-thread-cell-values))
                   (init-place-locals!)
                   (register-as-place-main!)
                   (async-callback-place-init!)
                   (let ([result (call/cc
                                  (lambda (esc)
                                    (set-box! place-esc-box esc)
                                    (thunk)
                                    0))])
                     (finish-proc result)
                     (collect-trip-for-allocating-places! -1)
                     (do-destroy-place)))))
  ;; Must be called within an engine, used for memory accounting:
  (define (current-place-roots)
    (list (place-registers)
          (current-engine-thread-cell-values)))]
 [else
  (define (place-enabled?) #f)
  (define (fork-place thunk finish-proc) #f)
  (define (current-place-roots) '())])

(define do-prepare-for-place void)
(define (set-prepare-for-place! proc)
  (set! do-prepare-for-place proc))

(define do-place-get-inherit (lambda () (list)))
(define (set-place-get-inherit! proc)
  (set! do-place-get-inherit proc))

(define do-start-place void)
(define (set-start-place! proc)
  (set! do-start-place proc))

(define do-destroy-place void)
(define (set-destroy-place! proc)
  (set! do-destroy-place proc))

(define (place-get-inherit)
  (do-place-get-inherit))

(define (start-place pch path sym in out err cust plumber inh)
  (let ([finish (do-start-place pch path sym in out err cust plumber inh)])
    (reset-async-callback-poll-wakeup!)
    finish))

(define (place-exit v)
  (let ([esc (unbox place-esc-box)])
    (if esc
        (esc v)
        (#%exit v))))

(define place-shared (make-weak-eq-hashtable))

(define (place-shared? v)
  (with-global-lock
   (hashtable-ref place-shared v #f)))

(define (register-place-shared v)
  (with-global-lock
   (hashtable-set! place-shared v #t))
  v)
