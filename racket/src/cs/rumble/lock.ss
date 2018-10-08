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

  ;; `eqv?`- and `eq?`-based tables appear to run with
  ;; interrupts disabled, so they're safe for engine-based
  ;; threads; just create a Racket-visible lock for
  ;; `equal?`-based hash tables
  (define (make-lock for-kind)
    (and (eq? for-kind 'equal?)
         (make-scheduler-lock)))

  (define lock-acquire
    (case-lambda ;; so it matches the one below
     [(lock)
      (when lock
        ;; Thread layer sets this callback to wait
        ;; on a semaphore:
        (scheduler-lock-acquire lock))]
     [(lock _)
      (when lock
        ;; Thread layer sets this callback to wait
        ;; on a semaphore:
        (scheduler-lock-acquire lock))]))
  
  (define (lock-release lock)
    (when lock
      (scheduler-lock-release lock)))

  ;; Use `with-global-lock*` when no lock is needed absent threads
  (define-syntax-rule (with-global-lock* e ...)
    (begin e ...))

  ;; Use `with-global-lock` when a lock is needed to prevent
  ;; engine-based concurrency
  (define-syntax-rule (with-global-lock e ...)
    (with-interrupts-disabled
     e ...))]
 [else
  ;; Using a Chez Scheme build with thread support; make hash-table
  ;; access thread-safe at that level for `eq?`- and `eqv?`-based
  ;; tables.
  ;; An `equal?`-based table is made safe at the level of Racket
  ;; threads, but not at Chez threads. Blocking a Chez thread might
  ;; block the Racket scheduler itself, so we just don't support it.

  ;; Assume low contention on `eq?`- and `eqv?`-based tables across
  ;; Chez Scheme threads, in which case a compare-and-set spinlock is
  ;; good enough.
  ;; Taking a lock disables interrupts, which ensures that the GC
  ;; callback or other atomic actions can use hash tables without
  ;; deadlocking.
  (define (make-spinlock) (box #f))
  (define (spinlock? v) (#%box? v))
  (define (spinlock-acquire q)
    (let loop ()
      (disable-interrupts)
      (unless (#%box-cas! q #f #t)
        (enable-interrupts)
        (loop))))
  (define (spinlock-release q)
    (#%set-box! q #f)
    (enable-interrupts)
    (#%void))

  (define (make-lock for-kind)
    (cond
     [(eq? for-kind 'equal?)
      (make-scheduler-lock)]
     [else
      (make-spinlock)]))

  (define lock-acquire
    (case-lambda
     [(lock)
      (cond
       [(not lock) (#%void)]
       [(spinlock? lock)
	(spinlock-acquire lock)]
       [else
	(scheduler-lock-acquire lock)])]
     [(lock block?)
      (cond
       [(not lock) (#%void)]
       [(spinlock? lock)
	(spinlock-acquire lock block?)]
       [else
	(scheduler-lock-acquire lock)])]))
  
  (define (lock-release lock)
    (cond
     [(not lock) (#%void)]
     [(spinlock? lock)
      (spinlock-release lock)]
     [else
      (scheduler-lock-release lock)]))

  (define global-lock (make-spinlock))
  (define-syntax-rule (with-global-lock* e ...)
    (with-global-lock e ...))
  (define-syntax-rule (with-global-lock e ...)
    (begin
      (spinlock-acquire global-lock)
      (begin0
       (begin e ...)
       (spinlock-release global-lock))))])
