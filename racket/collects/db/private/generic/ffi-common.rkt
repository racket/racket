#lang racket/base
(require racket/class
         racket/future
         racket/unsafe/ops
         ffi/unsafe/atomic
         ffi/unsafe/custodian
         ffi/unsafe/os-thread
         ffi/unsafe/os-async-channel
         "interfaces.rkt")
(provide (protect-out
          ffi-connection-mixin)
         worker-modes)

;; Convention: methods names starting with "-" usually indicate methods that
;; must be called in atomic mode.

(define (ffi-connection-mixin %)
  (class %
    (inherit call-with-lock)
    (super-new)

    ;; -get-db : -> DB/#f
    (abstract -get-db)

    ;; -get-do-disconnect : -> (-> (-> Void))
    ;; Partially disconnect (ie, mark as disconnected) and return a closure that
    ;; finishes disconnecting and returns a closure to report disconnection errors.
    ;; The first closure result may be called in an OS thread.
    ;; The second closure result is always called in a Racket thread (but maybe atomic).
    (abstract -get-do-disconnect)

    ;; ----------------------------------------
    ;; Disconnect

    (define/override (disconnect* _politely?)
      (super disconnect* _politely?)
      (real-disconnect))

    (define/public (real-disconnect)
      (call-as-atomic
       (lambda ()
         (when (-get-db)
           ;; Partially disconnect
           (define do-disconnect (-get-do-disconnect))
           ;; Finish disconnecting
           (cond [do-work
                  ;; Worker thread might be using db, stmts
                  (log-db-debug "disconnect delayed to worker thread")
                  (define finish (do-work do-disconnect #f))
                  (parameterize ((current-custodian (make-custodian-at-root)))
                    (thread
                     (lambda ()
                       (finish)
                       (log-db-debug "finished delayed disconnect"))))
                  (void)]
                 [else ((do-disconnect))])))))

    ;; ----------------------------------------
    ;; Worker Thread Support

    ;; do-work : #f or ((-> X) Boolean -> (-> X)), mutated
    (define do-work #f)

    ;; use-worker-mode : Symbol/#f -> Void
    ;; Should be called at most once, before connection is in use. Disconnect
    ;; assumes worker-mode is unchanging.
    (define/public (use-worker-mode mode)
      (when do-work
        (do-work void #f)
        (set! do-work #f))
      (case mode
        [(mailbox) (set! do-work (make-worker/mailbox))]
        [(queue) (set! do-work (make-worker/queue))]
        [(fqueue) (set! do-work (make-worker/fqueue))]
        [(os-thread) (set! do-work (make-worker/os-thread))]
        [(#f) (void)]
        [else (error 'use-worker-mode "unknown mode: ~e" mode)]))

    ;; worker-call : (Boolean -> X) -> X
    ;; Calls proc either normally or through worker. Argument to proc is #t if
    ;; in worker thread, #f if called normally.  To support error reporting from
    ;; OS threads, where parameters such as error-value->string-handler are not
    ;; available, if proc raises a procedure, then it is applied and the result
    ;; re-raised. (But usually the raised procedure itself will escape rather
    ;; than returning a value to raise.)
    (define/public (worker-call proc)
      (cond [do-work ((do-work (lambda () (proc #t)) #t))]
            [else (with-handlers ([procedure? (lambda (p) (raise (p)))])
                    (proc #f))]))
    ))

(define worker-modes
  (append (if (os-thread-enabled?) '(os-thread) '())
          (if (futures-enabled?) '(mailbox queue fqueue) '())))

;; ----------------------------------------

;; Worker request queues should be short: at most two elements (at most one from
;; lock holder, at most one from async custodian shutdown). So prefer simple
;; impl over better asymptotic complexity.

;; (Queue X) = (queue (U Semaphore FSemaphore) (Listof X))
;; List stored in insertion order.
(struct queue (sema [vs #:mutable]))
(define (queue-cas-vs! q old-vs new-vs)
  (unsafe-struct*-cas! q 1 old-vs new-vs))

;; Only used by worker threads, so doesn't need atomic/uninterruptible mode.
(define (queue-get q)
  (define sema (queue-sema q))
  (cond [(semaphore? sema) (semaphore-wait sema)]
        [(fsemaphore? sema) (fsemaphore-wait sema)])
  (let loop ()
    (define vs (queue-vs q))
    (if (queue-cas-vs! q vs (cdr vs)) (car vs) (loop))))

;; ----------------------------------------

(define (make-queue)
  (queue (make-semaphore 0) null))

(define (queue-put q v)
  (define sema (queue-sema q))
  (start-atomic)
  (let loop ()
    (define vs (queue-vs q))
    (unless (queue-cas-vs! q vs (append vs (list v)))
      (loop)))
  (semaphore-post sema)
  (end-atomic))

;; ----------------------------------------

(define (make-fqueue)
  (queue (make-fsemaphore 0) null))

(define (fqueue-put q v)
  (define sema (queue-sema q))
  (start-uninterruptible)
  (let loop ()
    (define vs (queue-vs q))
    (unless (queue-cas-vs! q vs (append vs (list v)))
      (loop)))
  (fsemaphore-post sema)
  (end-uninterruptible))

;; ----------------------------------------

;; make-worker/* : -> (-> X) Boolean -> (-> X)
;; 2nd request argument indicates whether worker should continue (#t) or quit (#f).

;; send will enter atomic
;; recv will sync
(define (make-worker/mailbox)
  (unless (futures-enabled?) (mode-not-supported 'mailbox))
  (define (handle-requests)
    (when ((thread-receive))
      (handle-requests)))
  (define handler-thread
    (parameterize ((current-custodian (make-custodian-at-root)))
      (thread #:pool 'own handle-requests)))
  (lambda (proc continue?)
    (define sema (make-semaphore 0))
    (define resultb (box #f))
    (define (wrapped-proc)
      (set-box! resultb (call/thunk proc))
      (semaphore-post sema)
      continue?)
    (thread-send handler-thread wrapped-proc #f)
    (lambda ()
      (semaphore-wait sema)
      ((unbox resultb)))))

;; send will enter atomic
;; recv will sync
(define (make-worker/queue)
  (unless (futures-enabled?) (mode-not-supported 'queue))
  (define req-q (make-queue))
  (define (handle-requests)
    (when ((queue-get req-q))
      (handle-requests)))
  (define handler-thread
    (parameterize ((current-custodian (make-custodian-at-root)))
      (thread #:pool 'own handle-requests)))
  (lambda (proc continue?)
    (define sema (make-semaphore 0))
    (define resultb (box #f))
    (define (wrapped-proc)
      (set-box! resultb (call/thunk proc))
      (semaphore-post sema)
      continue?)
    (queue-put req-q wrapped-proc)
    (lambda ()
      (semaphore-wait sema)
      ((unbox resultb)))))

;; send will enter uninterruptible
;; recv will fsemaphore-wait (if coroutine, sync via call-in-future)
(define (make-worker/fqueue)
  (unless (futures-enabled?) (mode-not-supported 'fqueue))
  (define req-fq (make-fqueue))
  (define (handle-requests)
    (when ((queue-get req-fq))
      (handle-requests)))
  (define handler-thread
    (parameterize ((current-custodian (make-custodian-at-root)))
      (thread #:pool 'own handle-requests)))
  (lambda (proc continue?)
    (define fsema (make-fsemaphore 0))
    (define resultb (box #f))
    (define (wrapped-proc)
      (set-box! resultb (call/thunk proc))
      (fsemaphore-post fsema)
      continue?)
    (fqueue-put req-fq wrapped-proc)
    (lambda ()
      (fsemaphore-wait fsema)
      ((unbox resultb)))))

;; send will enter atomic
;; recv will sync
(define (make-worker/os-thread)
  (unless (os-thread-enabled?) (mode-not-supported 'os-thread))
  (define req-os-chan (make-os-async-channel))
  (define (handle-requests)
    (when ((os-async-channel-get req-os-chan))
      (handle-requests)))
  (call-in-os-thread (lambda () (handle-requests)))
  (lambda (proc continue?)
    (define resp-os-chan (make-os-async-channel))
    (define (wrapped-proc)
      (os-async-channel-put resp-os-chan (call/thunk proc))
      continue?)
    (os-async-channel-put req-os-chan wrapped-proc)
    (lambda ()
      ((sync resp-os-chan)))))

;; call/thunk : (-> X) -> (-> X)
;; If proc raises a procedure, then the result thunk applies it and raises its
;; value. (See worker-call comment.)
(define (call/thunk proc)
  (with-handlers ([procedure? (lambda (p) (lambda () (raise (p))))]
                  [void (lambda (e) (lambda () (raise e)))])
    (let ([v (proc)]) (lambda () v))))

(define (mode-not-supported mode)
  (define msg (format "db connection: ~a mode not supported" mode))
  (raise (exn:fail:unsupported msg (current-continuation-marks))))
