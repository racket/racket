;; locking code for hash.ss

(define make-scheduler-lock (lambda () #f))
(define scheduler-lock-acquire (lambda (l) (#%void)))
(define scheduler-lock-release (lambda (l) (#%void)))

(define (set-scheduler-lock-callbacks! make acquire release)
  (set! make-scheduler-lock make)
  (set! scheduler-lock-acquire acquire)
  (set! scheduler-lock-release release))

(meta-cond
 [(not (threaded?))
  ;; Using a Chez Scheme build without thread support,
  ;; but we need to cooperate with engine-based threads.
  ;; Use `with-global-lock*` when no lock is needed absent threads
  (define-syntax-rule (with-global-lock* e ...)
    (begin e ...))

  ;; Use `with-global-lock` when a lock is needed to prevent
  ;; engine-based concurrency
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
    (begin
      (mutex-acquire global-lock)
      (begin0
       (begin e ...)
       (mutex-release global-lock))))])

;; ------------------------------------------------------------
;; Locks used for hash tables

(define (make-lock for-kind)
  (cond
   [(eq? for-kind 'equal?)
    (make-scheduler-lock)]
   [else #f]))

(define lock-acquire
  (case-lambda
   [(lock)
    (cond
     [(not lock) (disable-interrupts)]
     [else
      (scheduler-lock-acquire lock)])]))

(define (lock-release lock)
  (cond
   [(not lock) (enable-interrupts) (#%void)]
   [else
    (scheduler-lock-release lock)]))
