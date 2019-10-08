;; locking code for hash.ss

(define make-scheduler-lock (lambda () #f))
(define scheduler-lock-acquire (lambda (l) (#%void)))
(define scheduler-lock-release (lambda (l) (#%void)))

(define (set-scheduler-lock-callbacks! make acquire release)
  (set! make-scheduler-lock make)
  (set! scheduler-lock-acquire acquire)
  (set! scheduler-lock-release release))

;; Use `with-global-lock` to guard against both engine-based
;; concurrency and Scheme thread concurrency.

;; Use `with-global-lock*` when no guard against engine-based
;; concurrency is needed (because some operation is already atomic at
;; that level).

;; Taking the global lock disables GC, as does even waiting on the
;; lock. So, of course, make sure all globally-locked actions are
;; short.

(meta-cond
 [(not (threaded?))
  ;; Using a Chez Scheme build without thread support,
  ;; but we need to cooperate with engine-based threads.
  (define-syntax-rule (with-global-lock* e ...)
    (begin e ...))
  (define-syntax-rule (with-global-lock e ...)
    (with-interrupts-disabled
     e ...))]
 [else
  ;; Using a Chez Scheme build with thread support, so a stronger
  ;; global lock is needed.

  (define global-lock (make-mutex))
  (define-syntax-rule (with-global-lock* e ...)
    (with-global-lock e ...))
  (define-syntax-rule (with-global-lock e ...)
    ;; Disable interrupts before taking the lock, because some threads
    ;; may have interrupts disabled when they take the lock (which
    ;; blocks GCs), so always disabling interrupts imposes an order.
    (begin
      (disable-interrupts)
      (mutex-acquire global-lock)
      (begin0
       (begin e ...)
       (mutex-release global-lock)
       (enable-interrupts))))])

;; ------------------------------------------------------------
;; Locks used for hash tables

(define (make-lock for-kind)
  (cond
   [(eq? for-kind 'equal?)
    (make-scheduler-lock)]
   [else #f]))

(define (lock-acquire lock)
  (cond
   [(not lock) (disable-interrupts)]
   [else
    (scheduler-lock-acquire lock)]))

(define (lock-release lock)
  (cond
   [(not lock) (enable-interrupts) (#%void)]
   [else
    (scheduler-lock-release lock)]))
