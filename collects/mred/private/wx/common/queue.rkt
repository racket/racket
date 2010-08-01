#lang racket/base
(require ffi/unsafe
         racket/draw/utils
         ffi/unsafe/atomic
         "rbtree.rkt"
         "../../lock.rkt"
         "handlers.rkt")

(provide queue-evt
         set-check-queue!
         set-queue-wakeup!

         add-event-boundary-callback!
         add-event-boundary-sometimes-callback!
         remove-event-boundary-callback!
         pre-event-sync

         eventspace?
         current-eventspace
         queue-event
         yield
         (rename-out [make-new-eventspace make-eventspace])

         event-dispatch-handler
         eventspace-shutdown?
         main-eventspace?
         eventspace-handler-thread

         queue-callback
         middle-queue-key

         make-timer-callback
         add-timer-callback
         remove-timer-callback

         register-frame-shown
         get-top-level-windows

         queue-quit-event)

;; ------------------------------------------------------------
;; This module must be instantiated only once:

(define-mz scheme_register_process_global (_fun _string _pointer -> _pointer))
(let ([v (scheme_register_process_global "GRacket-support-initialized"
                                         (cast 1 _scheme _pointer))])
  (when v
    (error "cannot start GRacket a second time in the same process")))

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

(define (add-event-boundary-callback! v proc) 
  (hash-set! boundary-ht v proc))
(define (add-event-boundary-sometimes-callback! v proc) 
  (hash-set! sometimes-boundary-ht v proc))

(define (remove-event-boundary-callback! v) 
  (hash-remove! boundary-ht v)
  (hash-remove! sometimes-boundary-ht v))

(define last-time -inf.0)

;; Call this function only in atomic mode:
(define (pre-event-sync force?)
  (let ([now (current-inexact-milliseconds)])
    (when (or (now . > . (+ last-time 200)) 
              force?)
      (set! last-time now)
      (hash-for-each sometimes-boundary-ht 
                     (lambda (v p) (hash-remove! sometimes-boundary-ht v) (p v)))))
  (hash-for-each boundary-ht (lambda (v p) (hash-remove! boundary-ht v) (p v))))

;; ------------------------------------------------------------
;; Eventspaces

(define-struct eventspace (handler-thread queue-proc frames-hash done-evt)
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

(define (make-eventspace* th)
  (let ([done-sema (make-semaphore 1)]
        [frames (make-hasheq)])
    (make-eventspace th
                     (let ([count 0])
                       (let ([lo (mcons #f #f)]
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
                                       (semaphore-try-wait? done-sema)
                                       (semaphore-post done-sema)))]
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
                                             (first med)
                                             (first lo)
                                             (timer-first-wait timer)
                                             ;; nothing else ready...
                                             never-evt))])
                                 (end-atomic)
                                 e))]))))
                     frames
                     (semaphore-peek-evt done-sema))))

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

(define (handle-event thunk)
  (call-with-continuation-barrier
   (lambda ()
     (call-with-continuation-prompt thunk))))

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
       [handler?
        (sync (if (eq? evt 'wait) 
                  (wrap-evt e (lambda (_) #t))
                  evt)
              (handle-evt ((eventspace-queue-proc e))
                          (lambda (v)
                            (when v (handle-event v))
                            (yield evt))))]
       [else
        (sync e)]))]))

(define event-dispatch-handler (make-parameter void))
(define (eventspace-shutdown? e) #f)
(define (main-eventspace? e)
  (eq? e main-eventspace))

(define (queue-callback thunk [high? #f])
  (queue-event (current-eventspace) thunk (cond
                                           [(not high?) 'lo]
                                           [(eq? high? middle-queue-key) 'med]
                                           [else 'hi])))

(define middle-queue-key (gensym 'middle))


(define (add-timer-callback cb)
  (queue-event (current-eventspace) cb 'timer-add))
(define (remove-timer-callback cb)
  (queue-event (current-eventspace) cb 'timer-remove))

(define (register-frame-shown f on?)
  (queue-event (current-eventspace) f (if on?
                                          'frame-add
                                          'frame-remove)))

(define (get-top-level-windows)
  (hash-map (eventspace-frames-hash (current-eventspace))
            (lambda (k v) k)))

(define (queue-quit-event)
  ;; called in event-pump thread
  (queue-event main-eventspace (application-quit-handler) 'med))
