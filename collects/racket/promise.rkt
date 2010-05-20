#lang racket/base
(require "private/promise.rkt" (for-syntax racket/base))
(provide delay lazy force promise? promise-forced? promise-running?)

;; ----------------------------------------------------------------------------
;; More delay-like values, with different ways of deferring computations

(define-struct (promise/name promise) ()
  #:property prop:force (lambda (p) ((pref p))))
(provide (rename-out [delay/name* delay/name]))
(define delay/name make-promise/name)
(define-syntax (delay/name* stx) (make-delayer stx #'delay/name '()))

;; mostly to implement srfi-45's `eager'
(define-struct (promise/strict promise) ()
  #:property prop:force (lambda (p) (reify-result (pref p)))) ; never a thunk
(provide (rename-out [delay/strict* delay/strict]))
(define (delay/strict thunk)
  ;; could use `reify-result' here to capture exceptions too, or just create a
  ;; promise and immediately force it, but no point since if there's an
  ;; exception then the promise value is never used.
  (make-promise/strict (call-with-values thunk list)))
(define-syntax (delay/strict* stx) (make-delayer stx #'delay/strict '()))

;; utility struct
(define-struct (running-thread running) (thread))

;; used in promise/sync until it's forced
(define-struct syncinfo ([thunk #:mutable] done-evt done-sema access-sema))

(define-struct (promise/sync promise) ()
  #:property prop:custom-write
  (lambda (p port write?)
    (promise-printer
     (let ([v (pref p)])
       (if (syncinfo? v) (make-promise (syncinfo-thunk v)) p))
     port write?))
  #:property prop:force
  (lambda (p)
    (reify-result
     (let ([v (pref p)])
       (cond
         ;; already forced
         [(not (syncinfo? v)) v]
         ;; being forced...
         [(running-thread? (syncinfo-thunk v))
          (let ([r (syncinfo-thunk v)])
            (if (eq? (running-thread-thread r) (current-thread))
              ;; ... by the current thread => throw the usual reentrant error
              (r)
              ;; ... by a different thread => just wait for it
              (begin (sync (syncinfo-done-evt v)) (pref p))))]
         [else
          ;; wasn't forced yet: try to do it now
          (call-with-semaphore (syncinfo-access-sema v)
            (lambda ()
              (let ([thunk (syncinfo-thunk v)] [done (syncinfo-done-sema v)])
                ;; set the thread last
                (set-syncinfo-thunk!
                 v (make-running-thread (object-name thunk) (current-thread)))
                (call-with-exception-handler
                 (lambda (e)
                   (pset! p (make-reraise e))
                   (semaphore-post done)
                   e)
                 (lambda ()
                   (pset! p (call-with-values thunk list))
                   (semaphore-post done))))))
          ;; whether it was this thread that forced it or not, the results are
          ;; now in
          (pref p)]))))
  #:property prop:evt
  (lambda (p)
    (let ([v (pref p)])
      (handle-evt (if (syncinfo? v) (syncinfo-done-evt v) always-evt) void))))

(provide (rename-out [delay/sync* delay/sync]))
(define (delay/sync thunk)
  (let ([done-sema (make-semaphore 0)])
    (make-promise/sync (make-syncinfo thunk
                                      (semaphore-peek-evt done-sema) done-sema
                                      (make-semaphore 1)))))
(define-syntax (delay/sync* stx) (make-delayer stx #'delay/sync '()))

;; threaded promises

(define-struct (promise/thread promise) ()
  #:property prop:force
  (lambda (p)
    (reify-result (let ([v (pref p)])
                    (if (running-thread? v)
                      (begin (thread-wait (running-thread-thread v))
                             (pref p))
                      v))))
  #:property prop:evt
  (lambda (p)
    (let ([v (pref p)])
      (handle-evt (if (running? v) (running-thread-thread v) always-evt)
                  void))))

(provide (rename-out [delay/thread* delay/thread]))
(define (delay/thread thunk group)
  (unless (or (not group)
              (thread-group? group))
    (raise-type-error 'delay/thread "thread group" group))
  (let ()
    (define (run)
      (call-with-exception-handler
       (lambda (e) (pset! p (make-reraise e)) (kill-thread (current-thread)))
       (lambda () (pset! p (call-with-values thunk list)))))
    (define p
      (make-promise/thread
       (make-running-thread
        (object-name thunk)
        (if group
            (parameterize ([current-thread-group group]) (thread run))
            (thread run)))))
    p))
(define-syntax delay/thread*
  (let ([kwds (list (cons '#:group #'(make-thread-group)))])
    (lambda (stx) (make-delayer stx #'delay/thread kwds))))

(define-struct (promise/idle promise/thread) ()
  #:property prop:force
  (lambda (p)
    (reify-result (let ([v (pref p)])
                    (if (procedure? v)
                      ;; either running-thread, or returns the controller
                      (let ([controller (if (running-thread? v)
                                          (running-thread-thread v)
                                          (v))])
                        (thread-send controller 'force!)
                        (thread-wait controller)
                        (pref p))
                      v)))))

(provide (rename-out [delay/idle* delay/idle]))
(define (delay/idle thunk wait-for work-while tick use*)
  (unless (evt? wait-for)
    (raise-type-error 'delay/idle "evt" wait-for))
  (unless (evt? work-while)
    (raise-type-error 'delay/idle "evt" work-while))
  (unless (and (real? tick) (not (negative? tick)))
    (raise-type-error 'delay/idle "nonnegative real" tick))
  (unless (real? use*)
    (raise-type-error 'delay/idle "real" use*))
  (let ()
    (define use (cond [(use* . <= . 0) 0] [(use* . >= . 1) 1] [else use*]))
    (define work-time (* tick use))
    (define rest-time (- tick work-time))
    (define (work)
      (call-with-exception-handler
       (lambda (e) (pset! p (make-reraise e)) (kill-thread (current-thread)))
       (lambda () (pset! p (call-with-values thunk list)))))
    (define (run)
      ;; this thread is dedicated to controlling the worker thread, so it's
      ;; possible to dedicate messages to signaling a `force'.
      (define force-evt (thread-receive-evt))
      (sync wait-for force-evt)
      (pset! p (make-running-thread (object-name thunk) controller-thread))
      (let ([worker (parameterize ([current-thread-group (make-thread-group)])
                      (thread work))])
        (cond
         [(and (use . >= . 1) (equal? work-while always-evt))
          ;; as if it was pre-forced
          (thread-wait worker)]
         [(use . <= . 0)
          ;; work only when explicitly forced
          (thread-suspend worker)
          (sync force-evt)
          (thread-wait worker)]
         [else
          (thread-suspend worker)
          (let loop ()
            ;; rest, then wait for idle time, then resume working
            (if (eq? (begin0 (or (sync/timeout rest-time force-evt)
                                 (sync work-while force-evt))
                             (thread-resume worker))
                     force-evt)
                ;; forced during one of these => let it run to completion
                (thread-wait worker)
                ;; not forced
                (unless (sync/timeout work-time worker)
                  (thread-suspend worker)
                  (loop))))])))
    ;; I don't think that a thread-group here is needed, but it doesn't hurt
    (define controller-thread
      (parameterize ([current-thread-group (make-thread-group)])
        (thread run)))
    ;; the thunk is not really used in the above, make it a function that returns
    ;; the controller thread so it can be forced (used in the `prop:force')
    (define p (make-promise/idle
               (procedure-rename (lambda () controller-thread)
                                 (or (object-name thunk) 'idle-thread))))
    p))
(define-syntax delay/idle*
  (let ([kwds (list (cons '#:wait-for   #'(system-idle-evt))
                    (cons '#:work-while #'(system-idle-evt))
                    (cons '#:tick       #'0.2)
                    (cons '#:use        #'0.12))])
    (lambda (stx) (make-delayer stx #'delay/idle kwds))))
