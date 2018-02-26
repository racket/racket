#lang racket/base
(require "../common/check.rkt"
         "../../common/queue.rkt"
         "../host/thread.rkt"
         "../host/rktio.rkt"
         "../string/convert.rkt"
         "level.rkt"
         "logger.rkt")

(provide (struct-out log-receiver)
         make-log-receiver
         add-stderr-log-receiver!
         log-receiver-send!)

(struct log-receiver (filters))

(define-values (prop:receiver-send receiver-send? receiver-send-ref)
  (make-struct-type-property 'receiver-send))

;; ----------------------------------------

(struct queue-log-receiver log-receiver (msgs     ; queue of messages ready for `sync` [if `waiters` is null]
                                         waiters) ; queue of (box callback) to receive ready messages [if `msgs` is null]
  #:reflection-name 'log-receiver
  #:property
  prop:receiver-send
  (lambda (lr msg)
    ;; called in atomic mode and possibly in host interrupt handler,
    ;; so anything we touch here should only be modified with
    ;; interrupts disabled
    (atomically/no-interrupts
     (define b (queue-remove! (queue-log-receiver-waiters lr)))
     (cond
       [b
        (define select! (unbox b))
        (set-box! b msg)
        (select!)]
       [else
        (queue-add! (queue-log-receiver-msgs lr) msg)])))
  #:property
  prop:evt
  (poller (lambda (lr ctx)
            (define msg (atomically/no-interrupts (queue-remove! (queue-log-receiver-msgs lr))))
            (cond
              [msg
               (values (list msg) #f)]
              [else
               (define b (box (poll-ctx-select-proc ctx)))
               (define n (atomically/no-interrupts (queue-add! (queue-log-receiver-waiters lr) b)))
               (values #f (control-state-evt
                           (wrap-evt async-evt (lambda (e) (unbox b)))
                           (lambda () (atomically/no-interrupts (queue-remove-node! (queue-log-receiver-waiters lr) n)))
                           void
                           (lambda ()
                             (atomically/no-interrupts
                              (define msg (queue-remove! (queue-log-receiver-msgs lr)))
                              (cond
                                [msg
                                 (set-box! b msg)
                                 (values msg #t)]
                                [else
                                 (set! n (queue-add! (queue-log-receiver-waiters lr) b))
                                 (values #f #f)])))))]))))

(define/who (make-log-receiver logger level . args)
  (check who logger? logger)
  (define lr (queue-log-receiver (parse-filters 'make-log-receiver (cons level args) #:default-level 'none)
                                 (make-queue)
                                 (make-queue)))
  (add-log-receiver! logger lr)
  lr)

;; ----------------------------------------

(struct stderr-log-receiver log-receiver ()
  #:property
  prop:receiver-send
  (lambda (lr msg)
    ;; called in atomic mode and possibly in host interrupt handler
    (define fd (rktio_std_fd rktio RKTIO_STDERR))
    (define bstr (bytes-append (string->bytes/utf-8 (vector-ref msg 1)) #"\n"))
    (define len (bytes-length bstr))
    (let loop ([i 0])
      (define v (rktio_write_in rktio fd bstr i len))
      (unless (rktio-error? v)
        (let ([i (+ i v)])
          (unless (= i len)
            (loop i)))))
    (rktio_forget rktio fd)))

(define/who (add-stderr-log-receiver! logger . args)
  (check who logger? logger)
  (define lr (stderr-log-receiver (parse-filters 'make-stderr-log-receiver args #:default-level 'none)))
  (atomically
   (add-log-receiver! logger lr)
   (set-logger-permanent-receivers! logger (cons lr (logger-permanent-receivers logger)))))

;; ----------------------------------------

(define (add-log-receiver! logger lr)
  (atomically/no-interrupts
   ;; Add receiver to the logger's list, purning empty boxes
   ;; every time the list length doubles (roughly):
   (cond
     [(zero? (logger-prune-counter logger))
      (set-logger-receiver-boxes! logger (cons (make-weak-box lr)
                                               (for/list ([b (in-list (logger-receiver-boxes logger))]
                                                          #:when (weak-box-value b))
                                                 b)))
      (set-logger-prune-counter! logger (max 8 (length (logger-receiver-boxes logger))))]
     [else
      (set-logger-receiver-boxes! logger (cons (make-weak-box lr) (logger-receiver-boxes logger)))
      (set-logger-prune-counter! logger (sub1 (logger-prune-counter logger)))])
   ;; Increment the timestamp, so that wanted levels will be
   ;; recomputed on demand:
   (define ts-box (logger-root-level-timestamp-box logger))
   (set-box! ts-box (add1 (unbox ts-box)))
   ;; Post a semaphore to report that wanted levels may have
   ;; changed:
   (when (logger-level-sema logger)
     (semaphore-post (logger-level-sema logger))
     (set-logger-level-sema! logger #f))))

;; Called in atomic mode and with interrupts disabled
(define (log-receiver-send! r msg)
  ((receiver-send-ref r) r msg))
