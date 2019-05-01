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

  ;; Taking a lock disables interrupts, which ensures that the GC
  ;; callback or other atomic actions can use hash tables without
  ;; deadlocking.

  ;; Assume low contention on `eq?`- and `eqv?`-based tables across
  ;; Chez Scheme threads, in which case a compare-and-set spinlock is
  ;; usually good enough. But if not, transition to a real lock; use a
  ;; mutex, but transitioning requires using an inintermediate
  ;; semaphore.
  (define (make-spinlock)
    ;; Box content: #f (unlocked), #t (locked), sema (transitioning), or mutex
    (box #f))
  (define (spinlock? v) (#%box? v))
  (define (spinlock-acquire q)
    (let loop ([n 0])
      (disable-interrupts)
      (cond
       [(#%box-cas! q #f #t)
        ;; Took lock
        (#%void)]
       [(eq? #t (#%unbox q))
        ;; Spin..
        (enable-interrupts)
        (cond
         [(fx= n 1000)
          ;; There's contention after all, so trasition to a semaphore,
          ;; where the current lock holder implicitly owns the semaphore.
          ;; That lock holder can replace the semaphore with a mutex,
          ;; which is cheaper to acquire and release.
          (let ([lk (new-sema)])
            (#%box-cas! q #t lk)
            (loop 0))]
         [else
          (loop (fx+ n 1))])]
       [else
        (let ([l (#%unbox q)])
          (cond
           [(sema? l)
            ;; Transitioning to slower lock; wait on semaphore, then
            ;; try again
            (enable-interrupts)
            (sema-wait l)
            (loop 0)]
           [(mutex? l)
            ;; Using (permanent) mutex as lock
            (mutex-acquire l)]
           [else
            (enable-interrupts)
            (loop 0)]))])))

  (define (spinlock-release q)
    (unless (#%box-cas! q #t #f)
      ;; Contention must have promoted to a semaphore or mutex...
      (let ([l (#%unbox q)])
        (cond
         [(mutex? l)
          ;; Must have been acquired as a plain mutex
          (mutex-release l)]
         [else
          ;; Transitioning, so finish transition to a plain mutex
          (#%set-box! q (make-mutex))
          (sema-post-all l)])))
    (enable-interrupts)
    (#%void))

  ;; Semaphores that include a "post all" operation
  (define-record sema (v m c))
  (define (new-sema)
    (make-sema 0 (make-mutex) (make-condition)))
  (define (sema-wait l)
    (mutex-acquire (sema-m l))
    (let loop ()
      (let ([v (sema-v l)])
        (cond
         [(eqv? v #t) ; posted all
          (mutex-release (sema-m l))]
         [(eqv? 0 v)
          (condition-wait (sema-c l) (sema-m l))
          (loop)]
         [else
          (set-sema-v! l (sub1 v))
          (mutex-release (sema-m l))]))))
  (define (sema-post l)
    (mutex-acquire (sema-m l))
    (set-sema-v! l (add1 (sema-v l)))
    (condition-signal (sema-c l))
    (mutex-release (sema-m l)))
  (define (sema-post-all l)
    (mutex-acquire (sema-m l))
    (set-sema-v! l #t)
    (condition-broadcast (sema-c l))
    (mutex-release (sema-m l)))

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
