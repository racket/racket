#lang racket/base
(require "../common/check.rkt"
         "../../common/queue.rkt"
         "../host/thread.rkt"
         "../host/pthread.rkt"
         "../host/rktio.rkt"
         "../string/convert.rkt"
         "../path/system.rkt"
         "../path/path.rkt"
         "level.rkt"
         "logger.rkt")

(provide (struct-out log-receiver)
         make-log-receiver
         add-stderr-log-receiver!
         add-stdout-log-receiver!
         add-syslog-log-receiver!
         log-receiver-send!
         receiver-add-topics)

(struct log-receiver (filters))

(define-values (prop:receiver-send receiver-send? receiver-send-ref)
  (make-struct-type-property 'receiver-send))

;; ----------------------------------------

(struct queue-log-receiver log-receiver (msgs     ; queue of messages ready for `sync` [if `waiters` is null]
                                         waiters  ; queue of (box callback) to receive ready messages [if `msgs` is null]
                                         backref) ; box to make a self reference to avoid GC of a waiting receiver
  #:reflection-name 'log-receiver
  #:property
  prop:receiver-send
  (lambda (lr msg)
    ;; called in atomic mode
    (define b (queue-remove! (queue-log-receiver-waiters lr)))
    (cond
      [b
       (decrement-receiever-waiters! lr)
       (define select! (unbox b))
       (set-box! b msg)
       (select!)]
      [else
       (queue-add! (queue-log-receiver-msgs lr) msg)]))
  #:property
  prop:evt
  (poller (lambda (lr ctx)
            (define msg (queue-remove! (queue-log-receiver-msgs lr)))
            (cond
              [msg
               (values (list msg) #f)]
              [else
               (define b (box (poll-ctx-select-proc ctx)))
               (define n (begin
                           (increment-receiever-waiters! lr)
                           (queue-add! (queue-log-receiver-waiters lr) b)))
               (values #f (control-state-evt
                           (wrap-evt async-evt (lambda (e) (unbox b)))
                           (lambda ()
                             (queue-remove-node! (queue-log-receiver-waiters lr) n)
                             (decrement-receiever-waiters! lr))
                           void
                           (lambda ()
                             (define msg (queue-remove! (queue-log-receiver-msgs lr)))
                             (cond
                               [msg
                                (set-box! b msg)
                                (values msg #t)]
                               [else
                                (increment-receiever-waiters! lr)
                                (set! n (queue-add! (queue-log-receiver-waiters lr) b))
                                (values #f #f)]))))]))))

(define/who (make-log-receiver logger level . args)
  (check who logger? logger)
  (define backref (box #f))
  (define lr (queue-log-receiver (parse-filters 'make-log-receiver (cons level args) #:default-level 'none)
                                 (make-queue)
                                 (make-queue)
                                 backref))
  (add-log-receiver! logger lr backref)
  lr)


;; In atomic mode
(define (decrement-receiever-waiters! lr)
  (when (queue-empty? (queue-log-receiver-waiters lr))
    (set-box! (queue-log-receiver-backref lr) #f)))

;; In atomic mode
(define (increment-receiever-waiters! lr)
  (when (queue-empty? (queue-log-receiver-waiters lr))
    (set-box! (queue-log-receiver-backref lr) lr)))

;; ----------------------------------------

(struct stdio-log-receiver log-receiver (which)
  #:property
  prop:receiver-send
  (lambda (lr msg)
    ;; called in atomic mode and possibly in host interrupt handler
    (define fd (rktio_std_fd rktio (stdio-log-receiver-which lr)))
    (define bstr (bytes-append (string->bytes/utf-8 (vector-ref msg 1)) #"\n"))
    (define len (bytes-length bstr))
    (let loop ([i 0])
      (define v (rktio_write_in rktio fd bstr i len))
      (unless (rktio-error? v)
        (let ([i (+ i v)])
          (unless (= i len)
            (loop i)))))
    (rktio_forget rktio fd)))

(define (add-stdio-log-receiver! who logger args parse-who which)
  (check who logger? logger)
  (define lr (stdio-log-receiver (parse-filters parse-who args #:default-level 'none)
                                 which))
  (atomically
   (add-log-receiver! logger lr #f)
   (set-logger-permanent-receivers! logger (cons lr (logger-permanent-receivers logger)))))

(define/who (add-stderr-log-receiver! logger . args)
  (add-stdio-log-receiver! who logger args 'make-stderr-log-receiver RKTIO_STDERR))
  
(define/who (add-stdout-log-receiver! logger . args)
  (add-stdio-log-receiver! who logger args 'make-stdio-log-receiver RKTIO_STDOUT))

;; ----------------------------------------

(struct syslog-log-receiver log-receiver (cmd)
  #:property
  prop:receiver-send
  (lambda (lr msg)
    ;; called in atomic mode and possibly in host interrupt handler
    (define bstr (bytes-append (string->bytes/utf-8 (vector-ref msg 1)) #"\n"))
    (define pri
      (case (vector-ref msg 0)
        [(fatal) RKTIO_LOG_FATAL]
        [(error) RKTIO_LOG_ERROR]
        [(warning) RKTIO_LOG_WARNING]
        [(info) RKTIO_LOG_INFO]
        [else RKTIO_LOG_DEBUG]))
    (rktio_syslog rktio pri #f bstr (syslog-log-receiver-cmd lr))))

(define/who (add-syslog-log-receiver! logger . args)
  (define lr (syslog-log-receiver (parse-filters 'make-syslog-log-receiver args #:default-level 'none)
                                  (path-bytes (find-system-path 'run-file))))
  (atomically
   (add-log-receiver! logger lr #f)
   (set-logger-permanent-receivers! logger (cons lr (logger-permanent-receivers logger)))))

;; ----------------------------------------

(define (add-log-receiver! logger lr backref)
  (atomically/no-interrupts/no-wind
   ;; Add receiver to the logger's list, pruning empty boxes
   ;; every time the list length doubles (roughly):
   (cond
     [(zero? (logger-prune-counter logger))
      (set-logger-receiver-box+backrefs! logger
                                         (cons (cons (make-weak-box lr) backref)
                                               (for/list ([b+r (in-list (logger-receiver-box+backrefs logger))]
                                                          #:when (weak-box-value (car b+r)))
                                                 b+r)))
      (set-logger-prune-counter! logger (max 8 (length (logger-receiver-box+backrefs logger))))]
     [else
      (set-logger-receiver-box+backrefs! logger (cons (cons (make-weak-box lr) backref)
                                                      (logger-receiver-box+backrefs logger)))
      (set-logger-prune-counter! logger (sub1 (logger-prune-counter logger)))])
   ;; Increment the timestamp, so that wanted levels will be
   ;; recomputed on demand:
   (define ts-box (logger-root-level-timestamp-box logger))
   (set-box! ts-box (add1 (unbox ts-box)))
   ;; Post a semaphore to report that wanted levels may have
   ;; changed:
   (define sema-box (logger-level-sema-box logger))
   (when (unbox sema-box)
     (semaphore-post (unbox sema-box))
     (set-box! sema-box #f))))

;; Called in atomic mode and with interrupts disabled
(define (log-receiver-send! r msg in-interrupt?)
  (if (or (not in-interrupt?)
          ;; We can run stdio loggers in atomic/interrupt mode:
          (stdio-log-receiver? r))
      ((receiver-send-ref r) r msg)
      ;; Record any any other message for posting later:
      (unsafe-add-pre-poll-callback! (lambda ()
                                       ((receiver-send-ref r) r msg)))))

;; ----------------------------------------

(define (receiver-add-topics r topics default-level)
  (let loop ([filters (log-receiver-filters r)] [topics topics])
    (cond
      [(pair? filters)
       (loop (cdr filters) (hash-set topics (caar filters) #t))]
      [else
       (values topics (level-max default-level filters))])))
