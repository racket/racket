#lang racket/base

(require racket/contract/base
         racket/contract/combinator
         racket/generic)

;; This library implements buffered channels with
;; and optional buffer limit (so that puts block
;; if the buffer is full).

(define-generics async-channel-type
  (async-channel-get async-channel-type)
  (async-channel-try-get async-channel-type)
  (async-channel-put async-channel-type v)
  (async-channel-put-evt async-channel-type v))

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

(define (-async-channel-get ac)
  (sync ac))

(define (-async-channel-try-get ac)
  (sync/timeout 0 ac))

;; Put ----------------------------------------

(define (-async-channel-put-evt ac v)
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

(define (-async-channel-put ac v)
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

;; Struct definition -------------------------------------
;; needs to be down here because the generic methods need
;; to be defined

(struct ac (enqueue-ch dequeue-ch empty-ch full-ch thread)
  #:property prop:evt (lambda (ac)
                        (async-channel-get-guard ac))
  #:methods gen:async-channel-type
  [(define async-channel-get -async-channel-get)
   (define async-channel-try-get -async-channel-try-get)
   (define async-channel-put -async-channel-put)
   (define async-channel-put-evt -async-channel-put-evt)]
  #:reflection-name 'async-channel
  #:constructor-name make-ac)
(define async-channel? ac?)

;; Impersonators and Chaperones ---------------------------

(define (impersonate-async-channel ac get-proc put-proc . props)
  (impersonate-generics
   gen:async-channel-type
   ; there's no impersonate-evt...?
   (apply chaperone-evt
          ac (λ (evt) (values evt (λ (v) (get-proc v))))
          props)
   [async-channel-put
    (λ (async-channel-put)
      (chaperone-procedure async-channel-put
                           (λ (ac v) (values ac (put-proc v)))))]
   [async-channel-put-evt
    (λ (async-channel-put-evt)
      (chaperone-procedure async-channel-put-evt
                           (λ (ac v) (values ac (put-proc v)))))]))

(define (chaperone-async-channel ac get-proc put-proc . props)
  (chaperone-generics
   gen:async-channel-type
   (apply chaperone-evt
          ac (λ (evt) (values evt (λ (v) (get-proc v))))
          props)
   [async-channel-put
    (λ (async-channel-put)
      (chaperone-procedure async-channel-put
                           (λ (ac v) (values ac (put-proc v)))))]
   [async-channel-put-evt
    (λ (async-channel-put-evt)
      (chaperone-procedure async-channel-put-evt
                           (λ (ac v) (values ac (put-proc v)))))]))

;; Contracts -----------------------------------------------

(define (async-channel/c-name ctc)
  (define elem-name (contract-name (base-async-channel/c-content ctc)))
  (apply build-compound-type-name
         'async-channel/c
         elem-name
         '()))

(define (add-async-channel-context blame)
  (blame-add-context blame "a value passed through"))

(define (check-async-channel/c ctc val blame)
  (unless (async-channel? val)
    (raise-blame-error blame val '(expected "an async channel" given: "~e") val)))

(define (check-async-channel/c-np ctc val blame)
  (if (async-channel? val)
      #f
      (λ (neg-party)
        (raise-blame-error blame #:missing-party neg-party
                           val '(expected "an async channel" given: "~e") val))))

(define ((async-channel/c-first-order ctc) val)
  (define elem-ctc (base-async-channel/c-content ctc))
  (and (async-channel? val)
       (contract-first-order-passes? elem-ctc val)))

(define (async-channel/c-stronger? a b)
  (contract-stronger? (base-async-channel/c-content a) (base-async-channel/c-content b)))

(define ((ho-val-first-projection impersonate/chaperone-async-channel) ctc)
  (define elem-ctc (base-async-channel/c-content ctc))
  (define vfp (get/build-val-first-projection elem-ctc))
  (λ (blame)
    (define async-channel-blame (add-async-channel-context blame))
    (define pos-elem-proj (vfp async-channel-blame))
    (define neg-elem-proj (vfp (blame-swap async-channel-blame)))
    (λ (val)
      (or (check-async-channel/c-np ctc val blame)
          (λ (neg-party)
            (impersonate/chaperone-async-channel 
             val
             (λ (v) ((pos-elem-proj v) neg-party))
             (λ (v) ((neg-elem-proj v) neg-party))
             impersonator-prop:contracted ctc
             impersonator-prop:blame (blame-add-missing-party blame neg-party)))))))

(define ((ho-projection impersonate/chaperone-async-channel) ctc)
  (let ([elem-ctc (base-async-channel/c-content ctc)])
    (λ (blame)
      (let ([pos-elem-proj ((contract-projection elem-ctc) blame)]
            [neg-elem-proj ((contract-projection elem-ctc) (blame-swap blame))])
        (λ (val)
          (check-async-channel/c ctc val blame)
          (impersonate/chaperone-async-channel val pos-elem-proj neg-elem-proj
                                               impersonator-prop:contracted ctc
                                               impersonator-prop:blame blame))))))

(struct base-async-channel/c (content))

(struct chaperone-async-channel/c base-async-channel/c ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name async-channel/c-name
   #:first-order async-channel?
   #:stronger async-channel/c-stronger?
   #:val-first-projection (ho-val-first-projection chaperone-async-channel)
   #:projection (ho-projection chaperone-async-channel)))

(struct impersonator-async-channel/c base-async-channel/c ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:name async-channel/c-name
   #:first-order async-channel?
   #:stronger async-channel/c-stronger?
   #:val-first-projection (ho-val-first-projection impersonate-async-channel)
   #:projection (ho-projection impersonate-async-channel)))

(define (async-channel/c elem)
  (define ctc (coerce-contract 'async-channel/c elem))
  (if (chaperone-contract? ctc)
      (chaperone-async-channel/c ctc)
      (impersonator-async-channel/c ctc)))

;; Provides ----------------------------------------

(provide async-channel? async-channel/c)
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
                  (async-channel-put-evt (async-channel? any/c . -> . evt?))
                  (impersonate-async-channel ((async-channel?
                                               (any/c . -> . any/c)
                                               (any/c . -> . any/c))
                                              #:rest (listof any/c)
                                              . ->* . (and/c chaperone? async-channel?)))
                  (chaperone-async-channel ((async-channel?
                                             (any/c . -> . any/c)
                                             (any/c . -> . any/c))
                                            #:rest (listof any/c)
                                            . ->* . (and/c chaperone? async-channel?))))
