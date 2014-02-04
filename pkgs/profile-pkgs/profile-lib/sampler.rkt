#lang racket/base

;; The core profiler sample collector
;; (This module is a private tool for collecting profiling data, and should not
;; be used as is.)

(provide create-sampler)

(require errortrace/errortrace-key)

;; (cons sexp srcloc) -> (cons sexp srcloc)
;; abbreviate the expression for concise reports
;; we take the first symbol we can find, and wrap it in a stub expression
(define (errortrace-preprocess frame)
  (cons (and (car frame)
             (let loop ([e (car frame)])
               (cond [(symbol? e) (list e '...)]
                     [(pair? e) (loop (car e))]
                     [else (error 'errortrace-preprocess
                                  "unexpected frame: ~a" frame)])))
        (and (cdr frame)
             (apply srcloc (cdr frame)))))

;; create-sampler : creates a sample collector thread, which tracks the given
;; `to-track' value every `delay' seconds.
;; Uses errortrace annotations when #:use-errortrace? is specified, otherwise
;;   uses the native stack traces provided by `cms->context`.
;; * The input value can be either a thread (track just that thread), a
;;   custodian (track all threads managed by the custodian), or a list of
;;   threads and/or custodians.  If a custodian is given, it must be
;;   subordinate to the current custodian or to the given super custodian.
;;   Also optionally takes a list of continuation mark keys, which will be
;;   monitored in addition to the stack trace continuation mark key.
;; * The collected values are (<thread-id> <thread-time> . <stack>), where
;;   - The <thread-id> is an integer number identifying the thread, starting
;;     from 0.  If the collected data has thread ids in a 0..N range
;;     (exclusive) , then there were N threads that were observed.  (This can
;;     be relevant when tracking a custodian, where threads can change
;;     dynamically.)
;;   - The <thread-time> is the result of `current-process-milliseconds' for
;;     the thread that this sample came from.  Note that these numbers will not
;;     start at 0, since threads are likely to run before a sample is
;;     collected.
;;   - Finally, the <stack> part is a snapshot of the thread's stack, as
;;     grabbed by `continuation-mark-set->context'.  The values in these
;;     snapshots are interned to reduce memory load.
;;   The results are collected sequentially, so they're always sorted from the
;;   newest to the oldest.  Remember that these results should be considered
;;   private for the profiler collection, and can change when more information
;;   needs to be collected.
;; * Returns a "controller" function that accepts messages to control the
;;   sampler thread.  The current set of messages that the controller
;;   understands are:
;;   - 'pause and 'resume: stops or resumes collecting samples.  These messages
;;     can be nested.  Note that the thread will continue running it just won't
;;     collect snapshots.
;;   - 'stop: kills the collector thread.  Should be called when you have your
;;     data.  (There is no message to start a new sampler thread, although
;;     adding one will not be difficult.)
;;   - 'set-tracked! <new>: changes the thread/s and/or custodian/s to track.
;;     (Custodians should still be subordinate to the original one or to the
;;     given argument.)
;;   - 'set-delay! <new>: changes the sampling delay.  This means that we won't
;;     have a direct correlation between the number of samples and the time
;;     they represent -- but the samples are statistical snapshots anyway, and
;;     the results are not formulated in terms of time spent.  (The time spent
;;     could be added of course, but it is best to do that in terms of the
;;     start/stop times)
;;   - 'get-snapshots: returns the currently collected list of snapshots.  Note
;;     that this can be called multiple times, each will return the data that
;;     is collected up to that point in time.
;;   - 'get-custom-snapshots: returns the currently collected list of custom
;;     key snapshots. Returns a list of samples, where each sample is in the
;;     same format as the output of continuation-mark-set->list*.
(define (create-sampler to-track delay
                        [super-cust  (current-custodian)]
                        [custom-keys #f]
                        #:use-errortrace? [do-errortrace #f])
  ;; the collected data
  (define snapshots '())
  ;; listof (cons continuation-mark-key value/#f)
  (define custom-snapshots '())
  ;; intern the entries (which are (cons id/#f srcloc/#f))
  (define entry-table (make-hash))
  (define (intern-entry entry)
    (define key (or (cdr entry) (car entry)))
    (define en  (hash-ref entry-table key #f))
    (if en
      ;; ELI: is this sanity check needed?
      ;; (if (equal? en entry)
      ;;   en
      ;;   (error 'profile "internal error: assumption invalid"))
      en
      (begin (hash-set! entry-table key entry) entry)))
  (define (validate to-track who)
    (unless (or (not custom-keys) (list? custom-keys))
      (raise-type-error
       who "list of continuation mark keys" custom-keys))
    (let loop ([t to-track])
      (cond
        [(thread? t)]
        [(list? t) (for-each loop t)]
        [(not (custodian? t))
         (raise-type-error
          who "thread, custodian, or a list of threads/custodians" to-track)]
        ;; test that it's subordinate
        [(with-handlers ([exn:fail:contract? (λ (_) #t)])
           (custodian-managed-list t super-cust) #f)
         (error who "got an insubordinate custodian")])))
  (define paused 0)
  (define thread-id
    (let ([next-id 0] [t (make-weak-hasheq)])
      (λ (thd)
        (or (hash-ref t thd #f)
            (let ([id next-id])
              (set! next-id (add1 next-id))
              (hash-set! t thd id)
              id)))))
  (define (sampler)
    (sleep delay)
    (when (paused . <= . 0)
      (let loop ([t to-track])
        (cond [(thread? t)
               (unless (eq? t sampler-thread)
                 (when custom-keys
                   (set! custom-snapshots
                         (cons (continuation-mark-set->list*
                                (continuation-marks t)
                                custom-keys) ; frames
                               custom-snapshots)))
                 (set! snapshots
                       (cons (list* (thread-id t)
                                    (current-process-milliseconds t)
                                    (if do-errortrace
                                        (for/list ([frame (in-list
                                                           (continuation-mark-set->list
                                                            (continuation-marks t)
                                                            errortrace-key))])
                                            (intern-entry (errortrace-preprocess frame)))
                                        (map intern-entry
                                             (continuation-mark-set->context
                                              (continuation-marks t)))))
                             snapshots)))]
              [(custodian? t)
               (for-each loop (custodian-managed-list t super-cust))]
              ;; cannot assume that it's a list: we might get other values from
              ;; a custodian managed list
              [(list? t) (for-each loop t)])))
    (sampler))
  (define cpu-time   0)
  (define start-time (current-process-milliseconds))
  (define (add-time)
    (when (paused . <= . 0)
      (define cur (current-process-milliseconds))
      (set! cpu-time (+ cpu-time (- cur start-time)))
      (set! start-time cur)))
  (define (ignore-time)
    (when (paused . <= . 0)
      (set! start-time (current-process-milliseconds))))
  (define sampler-thread
    (begin (validate to-track 'create-sampler)
           (thread sampler)))
  ;; use a sema to avoid mutations from different threads, the sampler thread
  ;; is only reading these values so it doesn't use it.
  (define sema (make-semaphore 1))
  (define (sampler-controller msg [arg #f])
    (define-syntax-rule (w/sema body ...)
      (call-with-semaphore sema (λ () body ...)))
    (case msg
      [(pause)  (w/sema (add-time) (set! paused (add1 paused)))]
      [(resume) (w/sema (set! paused (sub1 paused)) (ignore-time))]
      [(stop)   (kill-thread sampler-thread) (add-time) (set! paused +inf.0)]
      [(set-tracked!) (validate arg 'sampler-controller)
                      (w/sema (set! to-track arg))]
      [(set-delay!)   (w/sema (set! delay arg))]
      [(get-snapshots) (add-time) (cons cpu-time snapshots)]
      [(get-custom-snapshots) custom-snapshots]
      [else (error 'sampler-controller "unknown message: ~e" msg)]))
  sampler-controller)
