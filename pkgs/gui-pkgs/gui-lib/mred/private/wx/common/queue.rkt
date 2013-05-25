#lang racket/base
(require ffi/unsafe
         racket/draw/private/utils
         ffi/unsafe/atomic
         racket/class
         racket/port
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
              set-platform-queue-sync!

              eventspace?
              current-eventspace
              queue-event
              queue-refresh-event
              yield
              yield/no-sync
              yield-refresh
              eventspace-event-evt
              (rename-out [make-new-eventspace make-eventspace])

              event-dispatch-handler
              eventspace-shutdown?
              main-eventspace?
              eventspace-handler-thread
              eventspace-event-evt
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
              queue-about-event
              queue-file-event
              queue-start-empty-event

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
                                (_fun #:atomic? #t _scheme _gcpointer -> _void)
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

;; This table refers to handle threads of eventspaces
;;  that have an open window, etc., so that the eventspace
;;  isn't GCed
(define active-eventspaces (make-hasheq))

(define current-cb-box (make-parameter #f))

(define-mz scheme_add_managed (_fun _racket ; custodian
                                    _racket ; object
                                    (_fun #:atomic? #t #:keep (lambda (v) (set-box! (current-cb-box) v))
                                          _racket _racket -> _void)
                                    _racket ; data
                                    _int ; strong?
                                    -> _gcpointer))

(define (shutdown-eventspace! e ignored)
  ;; atomic mode
  (unless (eventspace-shutdown? e)
    (set-eventspace-shutdown?! e #t)
    (semaphore-post (eventspace-done-sema e))
    (for ([f (in-list (get-top-level-windows e))])
      (send f destroy))
    (hash-remove! active-eventspaces (eventspace-handler-thread e))))

(define platform-queue-sync void)
(define (set-platform-queue-sync! proc)
  (set! platform-queue-sync proc))

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
                                                (hash-set! active-eventspaces th #t)
                                                (set! done-set? #f)
                                                (semaphore-try-wait? done-sema))
                                              (unless done-set?
                                                (hash-remove! active-eventspaces th)
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
                                       [first (lambda (q peek?)
                                                (and (mcar q)
                                                     (if peek?
                                                         always-evt
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
                                                              result))))))]
                                       [remove-timer
                                        (lambda (v timer)
                                          (set-box! timer (rbtree-remove 
                                                           timed-compare
                                                           v
                                                           (unbox timer)))
                                          (check-done))]
                                       [timer-first-ready
                                        (lambda (timer peek?)
                                          (let ([rb (unbox timer)])
                                            (and (not (null? rb))
                                                 (let* ([v (rbtree-min (unbox timer))]
                                                        [evt (timed-alarm-evt v)])
                                                   (and (sync/timeout 0 evt)
                                                        ;; It's ready
                                                        (if peek?
                                                            always-evt
                                                            (wrap-evt
                                                             always-evt
                                                             (lambda (_)
                                                               (start-atomic)
                                                               (remove-timer v timer)
                                                               (end-atomic)
                                                               (timed-val v)))))))))]
                                       [timer-first-wait
                                        (lambda (timer peek?)
                                          (let ([rb (unbox timer)])
                                            (and (not (null? rb))
                                                 (wrap-evt
                                                  (timed-alarm-evt (rbtree-min (unbox timer)))
                                                  (lambda (_) #f)))))]
                                       [make-event-choice
                                        (lambda (peek? sync?)
                                          (choice-evt
                                           (wrap-evt (semaphore-peek-evt newly-posted-sema)
                                                     (lambda (_) #f))
                                           (or (first hi peek?)
                                               (timer-first-ready timer peek?)
                                               (first refresh peek?)
                                               (first med peek?)
                                               (and (not peek?)
                                                    sync?
                                                    ;; before going with low-priority events,
                                                    ;; make sure we're sync'ed up with the
                                                    ;; GUI platform's event queue:
                                                    (platform-queue-sync)
                                                    (first med peek?))
                                               (first lo peek?)
                                               (timer-first-wait timer peek?)
                                               ;; nothing else ready...
                                               never-evt)))])
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
                                    (begin0 
                                     (make-event-choice #f #t)
                                     (end-atomic))]
                                   [(only-refresh? peek? sync?)
                                    (start-atomic)
                                    (begin0
                                     (cond
                                      [only-refresh?
                                       ;; Dequeue only refresh event
                                       (or (first refresh peek?) never-evt)]
                                      [else
                                       (make-event-choice peek? sync?)])
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
                            0))
      e)))

(define main-eventspace (make-eventspace* (current-thread)))
(define current-eventspace (make-parameter main-eventspace))

;; So we can get from a thread to the eventspace that
;;  it handles (independent of the `current-eventspace'
;;  parameter):
(define handler-thread-of (make-thread-cell #f))
(thread-cell-set! handler-thread-of main-eventspace)

(define make-new-eventspace
  (let ([make-eventspace
         (lambda ()
           (define pause (make-semaphore))
           (define break-paramz (current-break-parameterization))
           (define es
             (make-eventspace*
              (parameterize-break 
               #f ; disable breaks until we're in the yield loop
               (thread
                (lambda ()
                  (sync pause) ; wait until `es' has a value
                  (thread-cell-set! handler-thread-of es)
                  (current-eventspace es)
                  (let loop ()
                    (call-with-continuation-prompt
                     (lambda ()
                       ;; re-enable breaks (if they are supposed to be enabled):
                       (call-with-break-parameterization
                        break-paramz
                        (lambda () 
                          ;; yield; any abort (including a break exception)
                          ;; will get caught and the loop will yield again
                          (yield (make-semaphore))))))
                    (loop)))))))
           (semaphore-post pause) ; `es' has a value
           es)])
    make-eventspace))

(define (queue-event eventspace thunk [level 'med])
  ((eventspace-queue-proc eventspace) (cons level thunk)))

(define (queue-refresh-event eventspace thunk)
  ((eventspace-queue-proc eventspace) (cons 'refresh thunk)))

(define dispatch-event-prompt (make-continuation-prompt-tag))
(define dispatch-event-key (gensym))

(define (really-dispatch-event e)
  (let ([b (continuation-mark-set-first
            #f
            dispatch-event-key
            #f
            dispatch-event-prompt)])
    (unless b
      (error 'default-event-dispatch-handler
             "not in an event-dispatch context"))
    (let ([thunk (atomically
                  (begin0
                   (unbox b)
                   (set-box! b #f)))])
      (unless thunk
        (error 'default-event-dispatch-handler
               "event in current context was already dispatched"))
      (thunk))))

(define event-dispatch-handler (make-parameter really-dispatch-event))

(define event-logger (make-logger 'gui-event (current-logger)))
;; start? : boolean -- indicates if this is a start of an event being handled or not
;; msec : start time if start? is #t, delta from start to end if start? is #f
;; name : (or/c #f symbol?)
(struct gui-event (start end name) #:prefab)

(define (handle-event thunk e)
  (call-with-continuation-prompt ; to delimit continuations
   (lambda ()
     (call-with-continuation-prompt ; to delimit search for dispatch-event-key
      (lambda ()
        ;; communicate the thunk to `really-dispatch-event':
        (define before (current-inexact-milliseconds))
        (when (log-level? event-logger 'debug)
          (log-message event-logger 'debug 
                       (format "starting to handle an event from ~a" (object-name thunk))
                       (gui-event before #f (object-name thunk))))
        (let ([b (box thunk)])
          ;; use the event-dispatch handler:
          (with-continuation-mark dispatch-event-key b
            ((event-dispatch-handler) e))
          ;; if the event-dispatch handler doesn't chain
          ;; to the original one, then do so now:
          (when (unbox b)
            (set-box! b #f)
            (thunk)))
        (define after (current-inexact-milliseconds))
        (when (log-level? event-logger 'debug)
          (log-message event-logger 'debug 
                       (format "handled an event: ~a msec"  
                               (- after before))
                       (gui-event before after (object-name thunk)))))
      dispatch-event-prompt))))

(define yield
  (case-lambda
   [() 
    (let ([e (current-eventspace)])
      (if (eq? (current-thread) (eventspace-handler-thread e))
          (let ([v (sync/timeout 0 ((eventspace-queue-proc e)))])
            (if v
                (begin (handle-event v e) #t)
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
       [else
        (define (wait-now)
          (if handler?
              (sync (if (eq? evt 'wait) 
                        (wrap-evt e (lambda (_) #t))
                        evt)
                    (handle-evt ((eventspace-queue-proc e))
                                (lambda (v)
                                  (when v (handle-event v e))
                                  (yield evt))))
              (sync evt)))
        (if (evt? evt)
            ;; `yield' is supposed to return immediately if the
            ;; event is already ready:
            (sync/timeout wait-now evt)
            (wait-now))]))]))

(define (yield/no-sync)
  (let ([e (current-eventspace)])
    (when (eq? (current-thread) (eventspace-handler-thread e))
      (let ([v (sync/timeout 0 ((eventspace-queue-proc e) #f #f #f))])
        (if v
            (begin (handle-event v e) #t)
            #f)))))

(define yield-refresh
  (lambda ()
    (let ([e (current-eventspace)])
      (and (eq? (current-thread) (eventspace-handler-thread e))
           (let loop ([result #f])
             (let ([v (sync/timeout 0 ((eventspace-queue-proc e) #t #f #t))])
               (if v
                   (begin
                     (handle-event v e)
                     (loop #t))
                   result)))))))

(define (eventspace-event-evt [e (current-eventspace)])
  (unless (eventspace? e)
    (raise-type-error 'eventspace-event-evt "eventspace" e))
  (wrap-evt ((eventspace-queue-proc e) #f #t #t)
            (lambda (_) e)))

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
  (queue-event (send f get-eventspace) f (if on?
                                             'frame-add
                                             'frame-remove)))

(define (get-top-level-windows [e (current-eventspace)])
  ;; called in event-pump thread
  (hash-map (eventspace-frames-hash e)
            (lambda (k v) k)))

(define (other-modal? win [e #f] [ignore-win #f])
  ;; called in atomic mode in eventspace's thread
  (and
   ;; deliver mouse-motion events even if a modal window
   ;; is open
   (or (not e)
       (not (or (send e leaving?)
                (send e entering?)
                (send e moving?))))
   ;; for any other kind of mouse or key event, deliver only
   ;; if no model dialog is open
   (let ([es (send win get-eventspace)])
     (or (positive? (eventspace-external-modal es))
         (let loop ([frames (get-top-level-windows es)]) 
           (and (pair? frames)
                (let ([status (if (eq? ignore-win (car frames))
                                  #f
                                  (send (car frames) frame-relative-dialog-status win))])
                  (case status
                    [(#f) (loop (cdr frames))]
                    [(same) (loop (cdr frames))]
                    [(other) #t]))))))))

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

(define (queue-about-event)
  ;; called in event-pump thread
  (queue-event main-eventspace (application-about-handler) 'med))

(define (queue-file-event file)
  ;; called in event-pump thread
  (queue-event main-eventspace (lambda ()
                                 ((application-file-handler) file))
               'med))

(define (queue-start-empty-event)
  ;; called in event-pump thread
  (queue-event main-eventspace (application-start-empty-handler)
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

;; When using a REPL in a thread that has an eventspace,
;; yield to events when the port would block.
(current-get-interaction-input-port
 (let ([orig (current-get-interaction-input-port)])
   (lambda ()
     (let ([e (thread-cell-ref handler-thread-of)])
       (if e
           (let ([filter (lambda (v)
                           (cond
                            [(eq? v 0) (yield) 0]
                            [(evt? v)
                             (parameterize ([current-eventspace e])
                               (yield))
                             (choice-evt v
                                         (wrap-evt (eventspace-event-evt e)
                                                   (lambda (_) 0)))]
                            [else v]))])
             (filter-read-input-port
              (orig)
              (lambda (str v)
                (filter v))
              (lambda (s skip evt v)
                (filter v))))
           (orig))))))
