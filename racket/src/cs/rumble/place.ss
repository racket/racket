
;; The vector of place locals is similar to the set of virtual
;; registers, but the array can be shared by multiple Scheme threads
;; that are all in the same place.

;; The first slot in the vector holds a hash table for allocated
;; place-local values, and the rest are used by the thread, io, etc.,
;; layers for directly accessed variables.

(define NUM-PLACE-REGISTERS 128)

(define-virtual-register place-registers (#%make-vector NUM-PLACE-REGISTERS 0))
(define place-register-inits (#%make-vector NUM-PLACE-REGISTERS 0))

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
    (fork-thread (lambda ()
                   (init-virtual-registers)
                   (place-registers (vector-copy place-register-inits))
                   (root-thread-cell-values (make-empty-thread-cell-values))
                   (init-place-locals!)
                   (register-as-place-main!)
                   (let ([result (call/cc
                                  (lambda (esc)
                                    (set-box! place-esc-box esc)
                                    (thunk)
                                    0))])
                     (finish-proc result)))))
  ;; Must be called within an engine, used for memory accounting:
  (define (current-place-roots)
    (list (place-registers)
          (current-engine-thread-cell-values)))]
 [else
  (define (place-enabled?) #f)
  (define (fork-place thunk finish-proc) #f)
  (define (current-place-roots) '())])

(define do-start-place void)
(define (set-start-place! proc)
  (set! do-start-place proc))

(define (start-place pch path sym in out err cust plumber)
  (do-start-place pch path sym in out err cust plumber))

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
