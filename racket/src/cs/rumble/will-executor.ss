
;; Implements a variant of will executors with polling and a callback
;; for when a will becomes ready

(define-thread-local the-will-guardian (make-guardian))
(define-thread-local the-late-will-guardian (make-guardian #t))

;; Guardian callbacks are called fifo, but will executors are called
;; lifo. The `will-stacks` tables map a finalized value to a list
;; of finalizers, where each finalizer is an ephemeron pairing a will
;; executor with a will function (so that the function is not retained
;; if the will executor is dropped).
(define-thread-local the-will-stacks (make-weak-eq-hashtable))
(define-thread-local the-late-will-stacks (make-weak-eq-hashtable))

(define-thread-local late-will-executors-with-pending (make-eq-hashtable))

(define-record-type (will-executor create-will-executor will-executor?)
  (fields guardian will-stacks (mutable ready) notify keep?)
  (sealed #t))

(define (make-will-executor notify)
  (create-will-executor the-will-guardian the-will-stacks '() notify #f))

;; A "late" will executor corresponds to an ordered guardian. It
;; doesn't need to make any guarantees about order for multiple
;; registrations, so use a fresh guardian each time.
;; A late executor is treated a little specially in `will-register`.
(define make-late-will-executor
  (case-lambda
   [(notify) (make-late-will-executor notify #t)]
   [(notify keep?)
    (create-will-executor the-late-will-guardian the-late-will-stacks '() notify keep?)]))

(define/who (will-register executor v proc)
  (check who will-executor? executor)
  (check who (procedure-arity-includes/c 1) proc)
  (disable-interrupts)
  (let ([l (hashtable-ref (will-executor-will-stacks executor) v '())]
        ;; By using an ephemeron pair, if the executor becomes
        ;; unreachable, then we can drop the finalizer procedure. That
        ;; pattern prevents unbreakable cycles by an untrusted process
        ;; that has no access to a will executor that outlives the
        ;; process. But late will executors persist as long as
        ;; a will is registered.
        [e+proc (if (will-executor-keep? executor)
                    (cons executor proc)
                    (ephemeron-cons executor proc))])
    (hashtable-set! (will-executor-will-stacks executor) v (cons e+proc l))
    (when (null? l)
      ((will-executor-guardian executor) v)))
  (enable-interrupts)
  (void))

;; Returns #f or a pair: procedure and value
(define/who (will-try-execute executor)
  (check who will-executor? executor)
  (disable-interrupts)
  (poll-guardian (will-executor-guardian executor)
                 (will-executor-will-stacks executor))
  (let ([l (will-executor-ready executor)])
    (cond
     [(pair? l)
      (will-executor-ready-set! executor (cdr l))
      (when (and (will-executor-keep? executor)
                 (null? (cdr l)))
        (hashtable-delete! late-will-executors-with-pending executor))
      (enable-interrupts)
      (car l)]
     [else
      (enable-interrupts)
      #f])))

;; Call with interrupts disabled or from the thread scheduler
(define (poll-guardian guardian will-stacks)
  ;; Poll the guardian (which is shared among will executors)
  ;; for ready values, and add any ready value to the receiving will
  ;; executor
  (let loop ()
    (let ([v (guardian)])
      (when v
        (let we-loop ([l (hashtable-ref will-stacks v '())])
          (cond
           [(pair? l)
            (let* ([e+proc (car l)]
                   [e (car e+proc)]
                   [proc (cdr e+proc)]
                   [l (cdr l)])
              (cond
               [(eq? #!bwp e)
                ;; The will executor became inaccesible, so continue looking
                (we-loop l)]
               [else
                (cond
                 [(null? l)
                  (hashtable-delete! will-stacks v)]
                 [else
                  ;; Re-finalize for the next will registration
                  (hashtable-set! will-stacks v l)
                  (guardian v)])
                ((will-executor-notify e))
                (will-executor-ready-set! e (cons (cons proc v) (will-executor-ready e)))
                (when (will-executor-keep? e)
                  ;; Ensure that a late will executor stays live
                  ;; in this place as long as there are wills to execute
                  (hashtable-set! late-will-executors-with-pending e #t))]))]
           [else
            (hashtable-delete! will-stacks v)]))
        (loop)))))

(define (poll-will-executors)
  (poll-guardian the-will-guardian the-will-stacks)
  (poll-guardian the-late-will-guardian the-late-will-stacks))
