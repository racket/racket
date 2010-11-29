#lang racket/base
(require ffi/unsafe
         racket/draw/private/utils
         ffi/unsafe/atomic
         racket/class
         "rbtree.rkt"
         "../../lock.rkt"
         "handlers.rkt"
         "once.rkt")

(provide 
 (protect-out queue-evt
              set-check-queue!
              set-queue-wakeup!

              add-event-boundary-callback!
              add-event-boundary-sometimes-callback!
              remove-event-boundary-callback!
              pre-event-sync
              boundary-tasks-ready-evt
              sometimes-delay-msec

              eventspace?
              current-eventspace
              queue-event
              queue-refresh-event
              yield
              yield-refresh
              (rename-out [make-new-eventspace make-eventspace])

              event-dispatch-handler
              eventspace-shutdown?
              main-eventspace?
              eventspace-handler-thread
              eventspace-wait-cursor-count
              eventspace-extra-table
              eventspace-adjust-external-modal!

              queue-callback
              middle-queue-key

              make-timer-callback
              add-timer-callback
              remove-timer-callback

              register-frame-shown
              get-top-level-windows
              other-modal?

              queue-quit-event
              queue-prefs-event
              queue-file-event

              begin-busy-cursor
              end-busy-cursor
              is-busy?)

 scheme_register_process_global)

;; ------------------------------------------------------------
;; Create a Scheme evt that is ready when a queue is nonempty

(define _Scheme_Type _short)
(define-mz scheme_make_type (_fun _string -> _Scheme_Type))
(define event-queue-type (scheme_make_type "event-queue"))

(define-mz scheme_add_evt (_fun _Scheme_Type
                                (_fun #:atomic? #t _scheme -> _int)
                                (_fun #:atomic? #t _scheme _pointer -> _void)
                                _pointer
                                _int
                                -> _void))

(define (do-check-queue) #f)
(define (do-queue-wakeup fds) #f)

(define (check-queue o)
  (if (do-check-queue) 1 0))
(define (queue-wakeup o fds)
  (do-queue-wakeup fds))
(scheme_add_evt event-queue-type check-queue queue-wakeup #f 0)
(define queue-evt (let ([p (malloc 16)]
                        [p2 (malloc 'nonatomic _pointer)])
                    (memset p 0 16)
                    (ptr-set! p _Scheme_Type event-queue-type)
                    (ptr-set! p2 _pointer p)
                    (ptr-ref p2 _scheme)))

(define (set-check-queue! check)
  (set! do-check-queue check))
(define (set-queue-wakeup! wake)
  (set! do-queue-wakeup wake))

;; ------------------------------------------------------------
;; Pre-event sync

(define boundary-ht (make-hasheq))
(define sometimes-boundary-ht (make-hasheq))

(define tasks-ready? #f)
(define task-ready-sema (make-semaphore))
(define boundary-tasks-ready-evt (semaphore-peek-evt task-ready-sema))

(define (alert-tasks-ready)
  (let ([ready? (or (positive? (hash-count boundary-ht))
                    (positive? (hash-count sometimes-boundary-ht)))])
    (unless (eq? ready? tasks-ready?)
      (set! tasks-ready? ready?)
      (if ready?
          (semaphore-post task-ready-sema)
          (semaphore-wait task-ready-sema)))))

(define (add-event-boundary-callback! v proc)
  (atomically
   (hash-set! boundary-ht v proc)
   (alert-tasks-ready)))
(define (add-event-boundary-sometimes-callback! v proc) 
  (atomically
   (when (zero? (hash-count sometimes-boundary-ht))
     (set! last-time (current-inexact-milliseconds)))
   (hash-set! sometimes-boundary-ht v proc)
   (alert-tasks-ready)))

(define (remove-event-boundary-callback! v) 
  (atomically
   (hash-remove! boundary-ht v)
   (hash-remove! sometimes-boundary-ht v)
   (alert-tasks-ready)))

(define last-time -inf.0)
(define sometimes-delay-msec 100)

;; Call this function only in atomic mode:
(define (pre-event-sync force?)
  (let ([now (current-inexact-milliseconds)])
    (when (or (now . > . (+ last-time sometimes-delay-msec)) 
              force?)
      (set! last-time now)
      (hash-for-each sometimes-boundary-ht 
                     (lambda (v p) (hash-remove! sometimes-boundary-ht v) (p v)))))
  (hash-for-each boundary-ht (lambda (v p) (hash-remove! boundary-ht v) (p v)))
  (alert-tasks-ready))

;; ------------------------------------------------------------
;; Eventspaces

(define-struct eventspace (handler-thread 
                           queue-proc 
                           frames-hash 
                           done-evt 
                           [shutdown? #:mutable] 
                           done-sema 
                           [wait-cursor-count #:mutable]
                           extra-table
                           [external-modal #:mutable])
  #:property prop:evt (lambda (v)
                        (wrap-evt (eventspace-done-evt v)
                                  (lambda (_) v))))
(define-struct timed (alarm-evt msecs val [id #:mutable]))

(define (make-timer-callback msecs thunk)
  (make-timed (alarm-evt msecs)
              msecs
              thunk
              0))

(define (timed-compare a b)
  (if (eq? a b)
      0
      (let ([am (timed-msecs a)]
            [bm (timed-msecs b)])
        (cond
         [(= am bm) (if ((timed-id a) . < . (timed-id b))
                        -1
                        1)]
         [(< am bm) -1]
         [else 1]))))

(define current-cb-box (make-parameter #f))

(define-mz scheme_add_managed (_fun _racket ; custodian
                                    _racket ; object
                                    (_fun #:atomic? #t #:keep (lambda (v) (set-box! (current-cb-box) v))
                                          _racket _racket -> _void)
                                    _racket ; data
                                    _int ; strong?
                                    -> _pointer))

(define (shutdown-eventspace! e ignored)
  ;; atomic mode
  (unless (eventspace-shutdown? e)
    (set-eventspace-shutdown?! e #t)
    (semaphore-post (eventspace-done-sema e))
    (for ([f (in-list (get-top-level-windows e))])
      (send f destroy))))

(define (make-eventspace* th)
  (let ([done-sema (make-semaphore 1)]
        [done-set? #t]
        [frames (make-hasheq)])
    (let ([e
           (make-eventspace th
                            (let ([count 0])
                              (let ([lo (mcons #f #f)]
                                    [refresh (mcons #f #f)]
                                    [med (mcons #f #f)]
                                    [hi (mcons #f #f)]
                                    [timer (box '())]
                                    [timer-counter 0]
                                    [newly-posted-sema (make-semaphore)])
                                (let* ([check-done
                                        (lambda ()
                                          (if (or (positive? count)
                                                  (positive? (hash-count frames))
                                                  (not (null? (unbox timer))))
                                              (when done-set?
                                                (set! done-set? #f)
                                                (semaphore-try-wait? done-sema))
                                              (unless done-set?
                                                (set! done-set? #t)
                                                (semaphore-post done-sema))))]
                                       [enqueue (lambda (v q)
                                                  (set! count (add1 count))
                                                  (check-done)
                                                  (let ([p (mcons v #f)])
                                                    (if (mcdr q)
                                                        (set-mcdr! (mcdr q) p)
                                                        (set-mcar! q p))
                                                    (set-mcdr! q p)))]
                                       [first (lambda (q)
                                                (and (mcar q)
                                                     (wrap-evt
                                                      always-evt
                                                      (lambda (_)
                                                        (start-atomic)
                                                        (set! count (sub1 count))
                                                        (check-done)
                                                        (let ([result (mcar (mcar q))])
                                                          (set-mcar! q (mcdr (mcar q)))
                                                          (unless (mcar q)
                                                            (set-mcdr! q #f))
                                                          (end-atomic)
                                                          result)))))]
                                       [remove-timer
                                        (lambda (v timer)
                                          (set-box! timer (rbtree-remove 
                                                           timed-compare
                                                           v
                                                           (unbox timer)))
                                          (check-done))])
                                  (case-lambda
                                   [(v)
                                    ;; Enqueue
                                    (start-atomic)
                                    (let ([val (cdr v)])
                                      (case (car v)
                                        [(lo) (enqueue val lo)]
                                        [(refresh) (enqueue val refresh)]
                                        [(med) (enqueue val med)]
                                        [(hi) (enqueue val hi)]
                                        [(timer-add) 
                                         (set! timer-counter (add1 timer-counter))
                                         (set-timed-id! val timer-counter)
                                         (set-box! timer
                                                   (rbtree-insert
                                                    timed-compare
                                                    val
                                                    (unbox timer)))
                                         (check-done)]
                                        [(timer-remove) (remove-timer val timer)]
                                        [(frame-add) (hash-set! frames val #t) (check-done)]
                                        [(frame-remove) (hash-remove! frames val) (check-done)]))
                                    (semaphore-post newly-posted-sema)
                                    (set! newly-posted-sema (make-semaphore))
                                    (check-done)
                                    (end-atomic)]
                                   [()
                                    ;; Dequeue as evt
                                    (start-atomic)
                                    (let ([timer-first-ready
                                           (lambda (timer)
                                             (let ([rb (unbox timer)])
                                               (and (not (null? rb))
                                                    (let* ([v (rbtree-min (unbox timer))]
                                                           [evt (timed-alarm-evt v)])
                                                      (and (sync/timeout 0 evt)
                                                           ;; It's ready
                                                           (wrap-evt
                                                            always-evt
                                                            (lambda (_)
                                                              (start-atomic)
                                                              (remove-timer v timer)
                                                              (end-atomic)
                                                              (timed-val v))))))))]
                                          [timer-first-wait
                                           (lambda (timer)
                                             (let ([rb (unbox timer)])
                                               (and (not (null? rb))
                                                    (wrap-evt
                                                     (timed-alarm-evt (rbtree-min (unbox timer)))
                                                     (lambda (_) #f)))))])
                                      (let ([e (choice-evt
                                                (wrap-evt (semaphore-peek-evt newly-posted-sema)
                                                          (lambda (_) #f))
                                                (or (first hi)
                                                    (timer-first-ready timer)
                                                    (first refresh)
                                                    (first med)
                                                    (first lo)
                                                    (timer-first-wait timer)
                                                    ;; nothing else ready...
                                                    never-evt))])
                                        (end-atomic)
                                        e))]
                                   [(_1 _2)
                                    ;; Dequeue only refresh event
                                    (start-atomic)
                                    (begin0
                                     (or (first refresh) never-evt)
                                     (end-atomic))]))))
                            frames
                            (semaphore-peek-evt done-sema)
                            #f
                            done-sema
                            0
                            (make-hash)
                            0)]
          [cb-box (box #f)])
      (parameterize ([current-cb-box cb-box])
        (scheme_add_managed (current-custodian) 
                            e
                            shutdown-eventspace!
                            cb-box ; retain callback until it's called
                            1))
      e)))

(define main-eventspace (make-eventspace* (current-thread)))
(define current-eventspace (make-parameter main-eventspace))

(define make-new-eventspace
  (let ([make-eventspace
         (lambda ()
           (letrec ([pause (make-semaphore)]
                    [es
                     (make-eventspace*
                      (thread
                       (lambda ()
                         (sync pause)
                         (parameterize ([current-eventspace es])
                           (yield (make-semaphore))))))])
             (semaphore-post pause)
             es))])
    make-eventspace))

(define (queue-event eventspace thunk [level 'med])
  ((eventspace-queue-proc eventspace) (cons level thunk)))

(define (queue-refresh-event eventspace thunk)
  ((eventspace-queue-proc eventspace) (cons 'refresh thunk)))

(define (handle-event thunk)
  (let/ec esc
    (let ([done? #f])
      (dynamic-wind
       void
       (lambda ()
         (call-with-continuation-barrier
          (lambda ()
            (call-with-continuation-prompt thunk)))
         (set! done? #t))
       (lambda ()
         (unless done? (esc (void))))))))

(define yield
  (case-lambda
   [() 
    (let ([e (current-eventspace)])
      (if (eq? (current-thread) (eventspace-handler-thread e))
          (let ([v (sync/timeout 0 ((eventspace-queue-proc e)))])
            (if v
                (begin (handle-event v) #t)
                #f))
          #f))]
   [(evt)
    (unless (or (evt? evt)
                (eq? evt 'wait))
      (raise-type-error 'yield "evt or 'wait" evt))
    (let* ([e (current-eventspace)]
           [handler? (eq? (current-thread) (eventspace-handler-thread e))])
      (cond
       [(and (eq? evt 'wait)
             (not handler?))
        #t]
       ;; `yield' is supposed to return immediately if the
       ;; event is already ready:
       [(and (evt? evt) (sync/timeout 0 (wrap-evt evt (lambda (v) (list v)))))
        => (lambda (v) (car v))]
       [handler?
        (sync (if (eq? evt 'wait) 
                  (wrap-evt e (lambda (_) #t))
                  evt)
              (handle-evt ((eventspace-queue-proc e))
                          (lambda (v)
                            (when v (handle-event v))
                            (yield evt))))]
       [else
        (sync evt)]))]))

(define yield-refresh
  (lambda ()
    (let ([e (current-eventspace)])
      (and (eq? (current-thread) (eventspace-handler-thread e))
           (let loop ([result #f])
             (let ([v (sync/timeout 0 ((eventspace-queue-proc e) #f #f))])
               (if v
                   (begin
                     (handle-event v)
                     (loop #t))
                   result)))))))

(define event-dispatch-handler (make-parameter void))
(define (main-eventspace? e)
  (eq? e main-eventspace))

(define (queue-callback thunk [high? #t])
  (let ([es (current-eventspace)])
    (when (eventspace-shutdown? es)
      (error 'queue-callback "eventspace is shutdown: ~e" es))
    (queue-event es thunk (cond
                           [(not high?) 'lo]
                           [(eq? high? middle-queue-key) 'med]
                           [else 'hi]))))

(define middle-queue-key (gensym 'middle))


(define (add-timer-callback cb es)
  ;; in atomic mode
  (queue-event es cb 'timer-add))
(define (remove-timer-callback cb es)
  ;; in atomic mode
  (unless (eventspace-shutdown? es)
    (queue-event es cb 'timer-remove)))

(define (register-frame-shown f on?)
  (queue-event (current-eventspace) f (if on?
                                          'frame-add
                                          'frame-remove)))

(define (get-top-level-windows [e (current-eventspace)])
  ;; called in event-pump thread
  (hash-map (eventspace-frames-hash e)
            (lambda (k v) k)))

(define (other-modal? win)
  ;; called in atomic mode in eventspace's thread
  (let ([es (send win get-eventspace)])
    (or (positive? (eventspace-external-modal es))
        (let loop ([frames (get-top-level-windows es)]) 
          (and (pair? frames)
               (let ([status (send (car frames) frame-relative-dialog-status win)])
                 (case status
                   [(#f) (loop (cdr frames))]
                   [(same) #f]
                   [(other) #t])))))))

(define (eventspace-adjust-external-modal! es amt)
  (atomically
   (set-eventspace-external-modal! 
    es
    (+ (eventspace-external-modal es) amt))))

(define (queue-quit-event)
  ;; called in event-pump thread
  (queue-event main-eventspace (application-quit-handler) 'med))

(define (queue-prefs-event)
  ;; called in event-pump thread
  (queue-event main-eventspace (application-pref-handler) 'med))

(define (queue-file-event file)
  ;; called in event-pump thread
  (queue-event main-eventspace (lambda ()
                                 ((application-file-handler) file))
               'med))

(define (begin-busy-cursor) 
  (let ([e (current-eventspace)])
    (atomically 
     (set-eventspace-wait-cursor-count!
      e
      (add1 (eventspace-wait-cursor-count e)))
     (when (= (eventspace-wait-cursor-count e) 1)
       (for ([e (in-list (get-top-level-windows))])
         (send e set-wait-cursor-mode #t))))))

(define (end-busy-cursor) 
  (let ([e (current-eventspace)])
    (atomically 
     (set-eventspace-wait-cursor-count!
      e
      (sub1 (eventspace-wait-cursor-count e)))
     (when (zero? (eventspace-wait-cursor-count e))
       (for ([e (in-list (get-top-level-windows))])
         (send e set-wait-cursor-mode #f))))))

(define (is-busy?) (positive? (eventspace-wait-cursor-count (current-eventspace))))

;; ----------------------------------------

;; Before exiting, wait until frames are closed, etc.:
(executable-yield-handler
 (let ([old-eyh (executable-yield-handler)])
   (lambda (v)
     (yield main-eventspace)
     (old-eyh v))))
