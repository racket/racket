#lang racket/base
(require racket/class
         ffi/unsafe/atomic
         ffi/unsafe/custodian
         ffi/unsafe/os-thread
         ffi/unsafe/os-async-channel
         "interfaces.rkt")
(provide (protect-out
          ffi-connection-mixin))

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
           (cond [os-req-chan
                  ;; OS thread might be using db, stmts
                  (define resp-chan (make-os-async-channel))
                  (define (shutdown _db)
                    (define done (do-disconnect))
                    (when resp-chan (os-async-channel-put resp-chan done)))
                  (log-db-debug "disconnect delayed to OS thread")
                  (os-async-channel-put os-req-chan (cons shutdown #f))
                  (when resp-chan
                    (parameterize ((current-custodian (make-custodian-at-root)))
                      (thread
                       (lambda ()
                         (define done (sync resp-chan))
                         (log-db-debug "finished delayed disconnect")
                         (done)))))
                  (void)]
                 [else ((do-disconnect))])))))

    ;; ----------------------------------------
    ;; OS Thread Support

    (define use-os-thread? #f)
    (define os-req-chan #f)  ;; #f or OS-Async-Channel
    (define os-resp-chan #f) ;; #f or OS-Async-Channel

    (define/public (get-use-os-thread?) use-os-thread?)

    (define/public (use-os-thread use?)
      (when use?
        (unless (os-thread-enabled?)
          (raise (exn:fail:unsupported "use-os-thread: not supported"
                                       (current-continuation-marks)))))
      (call-with-lock 'use-os-thread
        (lambda ()
          (set! use-os-thread? (and use? #t))
          (when use?
            (call-as-atomic
             (lambda ()
               (unless os-req-chan
                 (define db (-get-db))
                 (define req-chan (make-os-async-channel))
                 (define resp-chan (make-os-async-channel))
                 (call-in-os-thread
                  (lambda ()
                    (let loop ()
                      (define msg (os-async-channel-get req-chan))
                      (define proc (car msg))
                      (define loop? (cdr msg))
                      (os-async-channel-put resp-chan (proc db))
                      (when loop? (loop)))))
                 (set! os-req-chan req-chan)
                 (set! os-resp-chan resp-chan))))))))

    ;; sync-call-in-os-thread : (DB/#f -> X) -> X
    ;; Calls proc either normally or in an OS thread. If in OS thread, proc is
    ;; passed the saved DB value (in case of partial disconnects); if called
    ;; normally, proc is passed #f.
    (define/public (sync-call-in-os-thread proc)
      (start-atomic)
      (cond [(-get-db)
             (when os-req-chan
               (os-async-channel-put os-req-chan (cons proc #t)))
             (end-atomic)]
            [else
             (end-atomic)
             (error/disconnect-in-lock 'sqlite3)])
      (sync os-resp-chan))

    ;; sync-call : (DB/#f -> X) -> X
    (define/public (sync-call proc)
      (cond [use-os-thread? (sync-call-in-os-thread proc)]
            [else (proc #f)]))
    ))
