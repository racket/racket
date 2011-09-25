#lang racket/base

(require racket/contract/base)

;; This library implements buffered channels with
;; and optional buffer limit (so that puts block
;; if the buffer is full).

;; We make a fancy structure just so an async-channel
;; can be supplied directly to `sync'.
;; The alternative is to use `define-struct' and supply
;; a `async-channel-get-evt' procedure.
(define-values (struct:ac make-ac async-channel? ac-ref ac-set!)
  (make-struct-type 'async-channel #f 5 0 #f 
                    (list (cons prop:evt 
                                ;; This is the guard that is called when
                                ;;  we use an async-channel as an event
                                ;;  (to get).
                                (lambda (ac)
                                  (async-channel-get-guard ac))))
                    (current-inspector) #f))
(define ac-enqueue-ch (make-struct-field-accessor ac-ref 0))
(define ac-dequeue-ch (make-struct-field-accessor ac-ref 1))
(define ac-empty-ch (make-struct-field-accessor ac-ref 2))
(define ac-full-ch (make-struct-field-accessor ac-ref 3))
(define ac-thread (make-struct-field-accessor ac-ref 4))

;; Make ----------------------------------------

(define make-async-channel
  (lambda ([limit #f])
    (let* ([enqueue-ch (make-channel)]   ; for puts
           [dequeue-ch (make-channel)]   ; for gets
           [empty-ch (make-channel)]     ; for get polls
           [full-ch (make-channel)]      ; for put polls
           [queue-first (mcons #f null)] ; queue head
           [queue-last queue-first]      ; queue tail
           [size 0]                      ; queue size
           ;; Events:
           [tell-empty 
            (channel-put-evt empty-ch (make-semaphore))] ; see poll->ch
           [tell-full 
            (channel-put-evt full-ch (make-semaphore))]  ; see poll->ch
           [enqueue (handle-evt
                     enqueue-ch
                     (lambda (v)
                       ;; We received a put; enqueue it:
                       (let ([p (mcons #f null)])
                         (set-mcar! queue-last v)
                         (set-mcdr! queue-last p)
                         (set! queue-last p)
                         (set! size (add1 size)))))]
           [mk-dequeue
            (lambda ()
              (handle-evt
               (channel-put-evt dequeue-ch (mcar queue-first))
               (lambda (ignored)
                 ;; A get succeeded; dequeue it:
                 (set! size (sub1 size))
                 (set! queue-first (mcdr queue-first)))))]
           [manager-thread
            ;; This thread is the part that makes the channel asynchronous.
            ;; It waits for a combination of gets and puts as appropriate.
            ;; Note that we start it with `thread/suspend-kill', and we
            ;; resume the manager thread with the current thread everytime
            ;; we want to talk to the manager thread, which effectively
            ;; means that the manager thread is not bound by a custodian
            ;; that is weaker than any of its user's custodians (and thus,
            ;; from the user's perspective, is not bound by any custodian
            ;; at all).
            (thread/suspend-to-kill
             (lambda ()
               (let loop ()
                 (cond
                  [(zero? size)
                   ;; The queue is currently empty:
                   (sync enqueue tell-empty)]
                  [(or (not limit) (size . < . limit))
                   (sync enqueue (mk-dequeue))]
                  [else
                   (sync (mk-dequeue) tell-full)])
                 (loop))))])
      (make-ac enqueue-ch dequeue-ch empty-ch full-ch manager-thread))))

;; Get ----------------------------------------

(define (async-channel-get-guard ac)
  ;; Make sure queue manager is running:
  (thread-resume (ac-thread ac) (current-thread))
  ;; If it the channel is being polled, it's not
  ;;  good enough to poll the dequeue channel, because
  ;;  the server thread may be looping. In that case,
  ;;  block on the dequeue channel and the empty
  ;;  channel, and create a new waitable to report
  ;;  the result.
  (poll-guard-evt
   (lambda (poll?)
     (if poll?
         (poll->ch (ac-dequeue-ch ac) (ac-empty-ch ac))
         (ac-dequeue-ch ac)))))

(define (async-channel-get ac)
  (sync ac))

(define (async-channel-try-get ac)
  (sync/timeout 0 ac))

;; Put ----------------------------------------

(define (async-channel-put-evt ac v)
  (letrec ([p (wrap-evt
               (guard-evt
                (lambda ()
                  ;; Make sure queue manager is running:
                  (thread-resume (ac-thread ac) (current-thread))
                  (let ([p (channel-put-evt (ac-enqueue-ch ac) v)])
                    ;; Poll handling, as in `async-channel-get-guard':
                    (poll-guard-evt
                     (lambda (poll?)
                       (if poll?
                           (poll->ch p (ac-full-ch ac))
                           p))))))
               (lambda (ignored) p))])
    p))

(define (async-channel-put ac v)
  (thread-resume (ac-thread ac) (current-thread))
  (sync (channel-put-evt (ac-enqueue-ch ac) v))
  (void))

;; Poll helper ----------------------------------------

(define (poll->ch normal not-ready)
  (sync
   ;; If a value becomes available,
   ;;  create a waitable that returns
   ;;  the value:
   (wrap-evt
    normal
    (lambda (v)
      ;; Return a waitable for a successful poll:
      (wrap-evt
       always-evt
       (lambda (ignored) v))))
   ;; If not-ready becomes available,
   ;;  the result is supposed to be
   ;;  a never-ready waitable:
   not-ready))

;; Provides ----------------------------------------

(provide async-channel?)
(provide/contract (make-async-channel (case->
                                       (-> async-channel?)
                                       ((or/c false/c (lambda (x)
                                                         (and (integer? x)
                                                              (exact? x)
                                                              (positive? x))))
                                        . -> . async-channel?)))
                  (async-channel-get (async-channel? . -> . any/c))
                  (async-channel-try-get (async-channel? . -> . any/c))
                  (async-channel-put (async-channel? any/c . -> . any/c))
                  (async-channel-put-evt (async-channel? any/c . -> . evt?)))
