#lang racket/base
(require racket/fixnum
         "../common/class.rkt"
         "../common/check.rkt"
         "../host/rktio.rkt"
         "../host/error.rkt"
         "../host/thread.rkt"
         "../host/pthread.rkt"
         "../sandman/main.rkt"
         "../sandman/ltps.rkt"
         "../file/error.rkt"
         "port.rkt"
         "input-port.rkt"
         "output-port.rkt"
         "lock.rkt"
         "peek-via-read-port.rkt"
         "file-stream.rkt"
         "file-truncate.rkt"
         "buffer-mode.rkt"
         "close.rkt"
         "count.rkt"
         "check.rkt")

(provide (struct-out fd-input-port)
         open-input-fd
         finish-fd-input-port
         (struct-out fd-output-port)
         open-output-fd
         finish-fd-output-port
         terminal-port?
         port-waiting-peer?
         fd-port-fd
         prop:fd-place-message-opener)

;; lock on `p` held, in rktio and rktio-sleep-relevant mode,
;; and with custodian lock unless errors are being discarded
(define (fd-close fd fd-refcount p
                  #:discard-errors? [discard-errors? #f])
  (set-box! fd-refcount (sub1 (unbox fd-refcount)))
  (when (zero? (unbox fd-refcount))
    (fd-semaphore-update! fd 'remove)
    (define v (rktio_close rktio fd))
    (when (and (rktio-error? v)
               (not discard-errors?))
      (unsafe-uninterruptible-custodian-lock-release)
      (end-rktio-sleep-relevant)
      (end-rktio)
      (port-unlock p)
      (raise-rktio-error #f v "error closing stream port"))))

;; ----------------------------------------

(class fd-input-port #:extends peek-via-read-input-port
  #:field
  [fd #f]
  [fd-refcount (box 1)] ; rktio-sleep-relevant mode serves as a lock for a refcount
  [custodian-reference #f]
  [is-converted #f]
  
  #:public
  [on-close (lambda () (void))] ; lock held and in rktio and rktio-sleep-relevant mode
  [raise-read-error (lambda (n)
                      (raise-filesystem-error #f n "error reading from stream port"))]

  #:override
  [read-in/inner
   (lambda (dest-bstr start end copy? to-buffer?)
     (rktioly
      (define n
        (cond
          [(and to-buffer?
                (rktio_fd_is_text_converted rktio fd))
           ;; need to keep track of whether any bytes in the buffer were converted
           (when (or (not is-converted)
                     ((bytes-length is-converted) . < . end))
             (define new-is-converted (make-bytes end))
             (when is-converted
               (bytes-copy! new-is-converted 0 is-converted))
             (set! is-converted new-is-converted))
           (rktio_read_converted_in rktio fd dest-bstr start end is-converted start)]
          [else
           (rktio_read_in rktio fd dest-bstr start end)]))
      (cond
        [(rktio-error? n)
         (end-rktio)
         (port-unlock this)
         (send fd-input-port this raise-read-error n)]
        [(eqv? n RKTIO_READ_EOF) eof]
        [(eqv? n 0) (or (fd-semaphore-update! fd 'read)
                        (fd-evt fd RKTIO_POLL_READ fd-refcount))]
        [else n])))]

  [byte-ready/inner
   (lambda (work-done!)
     (rktioly
      (cond
        [(eqv? (rktio_poll_read_ready rktio fd) RKTIO_POLL_READY)
         #t]
        [else (or (fd-semaphore-update! fd 'read)
                  (fd-evt fd RKTIO_POLL_READ fd-refcount))])))]

  [close
   (lambda ()
     (start-rktio)
     (unless (zero? (unbox fd-refcount)) ; double-check, since we left locked mode
       (start-rktio-sleep-relevant)
       (unsafe-uninterruptible-custodian-lock-acquire)
       (send fd-input-port this on-close)
       (fd-close fd fd-refcount this)
       (unsafe-custodian-unregister this custodian-reference)
       (unsafe-uninterruptible-custodian-lock-release)
       (end-rktio-sleep-relevant)
       (close-peek-buffer))
     (end-rktio))]

  [file-position
   (case-lambda
     [()
      (define pos (get-file-position fd))
      (and pos (buffer-adjust-pos pos is-converted))]
     [(pos)
      (purge-buffer)
      (set-file-position fd pos this)])]

  #:property
  [prop:file-stream (lambda (p) (fd-input-port-fd p))]
  [prop:place-message (lambda (port)
                        (lambda ()
                          (fd-port->place-message port)))])

;; ----------------------------------------

;; in uninterrupted mode or with custodian lock
;; current custodian must not be shut down
(define (open-input-fd fd name
                       #:fd-refcount [fd-refcount (box 1)]
                       #:custodian [cust (current-custodian)])
  (finish-fd-input-port
   (new fd-input-port
        #:field
        [name name]
        [fd fd]
        [fd-refcount fd-refcount])
   #:custodian cust))

;; in uninterrupted mode or with custodian lock
;; current custodian must not be shut down
(define (finish-fd-input-port p
                              #:custodian [cust (current-custodian)])
  (define fd (fd-input-port-fd p))
  (define fd-refcount (fd-input-port-fd-refcount p))
  (set-fd-input-port-custodian-reference! p (register-fd-close cust fd fd-refcount #f p))
  (finish-port/count p))

;; ----------------------------------------

(class fd-output-port #:extends core-output-port
  #:field
  [fd fd]
  [fd-refcount (box 1)] ; rktio mode serves as a lock for a refcount
  [bstr (make-bytes 4096)]
  [start-pos 0]
  [end-pos 0]
  [flush-handle #f]
  [buffer-mode 'block]
  [custodian-reference #f]

  #:static
  [fast-mode!
   (lambda (amt) ; amt = not yet added to `offset`
     (when (eq? buffer-mode 'block)
       (define b buffer)
       (define e end-pos)
       (set-direct-bstr! b bstr)
       (set-direct-pos! b e)
       (set-direct-end! b (bytes-length bstr))
       (define o offset)
       (when o
         (set! offset (- (+ o amt) e)))))]

  [slow-mode!
   (lambda ()
     (define b buffer)
     (when (direct-bstr b)
       (set-direct-bstr! b #f)
       (define pos (direct-pos b))
       (set! end-pos pos)
       (define o offset)
       (when o
         (set! offset (+ o pos)))
       (set-direct-pos! b (direct-end b))))]

  #:public
  [on-close (lambda () (void))]
  [raise-write-error
   (lambda (n)
     (raise-filesystem-error #f n "error writing to stream port"))]

  #:private
  ;; lock held
  ;; Returns `#t` if the buffer is already or successfully flushed
  [flush-buffer
   (lambda (no-escape?)
     (slow-mode!)
     (cond
       [(not (fx= start-pos end-pos))
        (define n (rktioly (rktio_write_in rktio fd bstr start-pos end-pos)))
        (cond
          [(rktio-error? n)
           ;; Discard buffer content before reporting the error. This
           ;; isn't the obviously right choice, but otherwise a future
           ;; flush attempt (including one triggered by trying to
           ;; close the port or one triggered by a plumber) will
           ;; likely just fail again, which is probably worse than
           ;; dropping bytes.
           (set! start-pos 0)
           (set! end-pos 0)
           (cond
             [no-escape?
              (lambda () (send fd-output-port this raise-write-error n))]
             [else
              (port-unlock this)
              (send fd-output-port this raise-write-error n)])]
          [(fx= n 0)
           #f]
          [else
           (define new-start-pos (fx+ start-pos n))
           (cond
             [(fx= new-start-pos end-pos)
              (set! start-pos 0)
              (set! end-pos 0)
              #t]
             [else
              (set! start-pos new-start-pos)
              #f])])]
       [else #t]))]

  ;; lock held, but may leave it temporarily
  [flush-buffer-fully
   (lambda (enable-break?)
     (let loop ()
       (unless (flush-buffer #f)
         (port-unlock this)
         (if enable-break?
             (sync/enable-break evt)
             (sync evt))
         (port-lock this)
         (when bstr ; in case it was closed
           (loop)))))]

  ;; lock held, but may leave it temporarily
  [flush-buffer-fully-if-newline
   (lambda (src-bstr src-start src-end enable-break?)
     (for ([b (in-bytes src-bstr src-start src-end)])
       (define newline? (or (eqv? b (char->integer #\newline))
                            (eqv? b (char->integer #\return))))
       (when newline? (flush-buffer-fully enable-break?))
       #:break newline?
       (void)))]

  ;; lock held, but may leave it temporarily
  [flush-rktio-buffer-fully
   (lambda ()
     (unless (rktioly (rktio-flushed?))
       (port-unlock this)
       (sync (rktio-fd-flushed-evt this))
       (port-lock this)
       (flush-rktio-buffer-fully)))]

  #:static
  [flush-buffer/external
   (lambda ()
     (flush-buffer-fully #f))]

  ;; in rktio mode
  [rktio-flushed?
   (lambda ()
     (or (not bstr)
         (rktio_poll_write_flushed rktio fd)))]

  #:override
  ;; lock held
  [write-out
   (lambda (src-bstr src-start src-end nonbuffer/nonblock? enable-break? copy? no-escape?)
     (slow-mode!)
     (cond
       [(fx= src-start src-end)
        ;; Flush request
        (or (let ([r (flush-buffer no-escape?)])
              (and r
                   (if (procedure? r)
                       r ; possible result in `no-escape?` mode
                       0)))
            (wrap-evt evt (lambda (v) #f)))]
       [(and (not (eq? buffer-mode 'none))
             (not nonbuffer/nonblock?)
             (fx< end-pos (bytes-length bstr)))
        (define amt (fxmin (fx- src-end src-start) (fx- (bytes-length bstr) end-pos)))
        (bytes-copy! bstr end-pos src-bstr src-start (fx+ src-start amt))
        (set! end-pos (fx+ end-pos amt))
        (when (eq? buffer-mode 'line)
          ;; can temporarily leave port lock:
          (flush-buffer-fully-if-newline src-bstr src-start src-end enable-break?))
        (fast-mode! amt)
        amt]
       [(not (flush-buffer no-escape?))
        (wrap-evt evt (lambda (v) #f))]
       [else
        (define n (rktio_write_in rktio fd src-bstr src-start src-end))
        (cond
          [(rktio-error? n)
           (cond
             [no-escape?
              (lambda () (send fd-output-port this raise-write-error n))]
             [else
              (port-unlock this)
              (send fd-output-port this raise-write-error n)])]
          [(fx= n 0) (wrap-evt evt (lambda (v) #f))]
          [else n])]))]

  [get-write-evt
   (get-write-evt-via-write-out (lambda (out v bstr start)
                                  (port-count! out v bstr start)))]

  ;; lock held
  [close
   (lambda ()
     (flush-buffer-fully #f) ; can temporarily leave lock
     (flush-rktio-buffer-fully) ; can temporarily leave lock
     (when bstr ; <- in case a concurrent close succeeded
       (start-rktio)
       (start-rktio-sleep-relevant)
       (unsafe-uninterruptible-custodian-lock-acquire)
       (when bstr ; check again, since we left locked mode again
         (send fd-output-port this on-close)
         (when flush-handle
           (plumber-flush-handle-remove! flush-handle))
         (set! bstr #f)
         (fd-close fd fd-refcount this)
         (unsafe-custodian-unregister this custodian-reference))
       (unsafe-uninterruptible-custodian-lock-release)
       (end-rktio-sleep-relevant)
       (end-rktio)))]

  ;; lock held
  [file-position
   (case-lambda
     [()
      (define pos (get-file-position fd))
      (define b buffer)
      (and pos (+ pos (fx- (if (direct-bstr b) (direct-pos b) end-pos) start-pos)))]
     [(pos)
      (flush-buffer-fully #f)
      ;; flushing can leave port lock, so make sure the
      ;; port is still open before continuing
      (unless bstr
        (check-not-closed 'file-position this))
      (set-file-position fd pos this)])]

  ;; lock held
  [buffer-mode
   (case-lambda
     [() buffer-mode]
     [(mode) (set! buffer-mode mode)])]

  #:property
  [prop:file-stream (lambda (p) (fd-output-port-fd p))]
  [prop:file-truncate (lambda (p pos)
                        ;; lock held and *not* in rktio mode
                        (send fd-output-port p flush-buffer/external)
                        (define result
                          (rktioly
                           (rktio_set_file_size rktio
                                                (fd-output-port-fd p)
                                                pos)))
                        (when (rktio-error? result)
                          (port-unlock p)
                          (raise-rktio-error 'file-truncate result  "error setting file size")))]
  [prop:place-message (lambda (port)
                        (lambda ()
                          (fd-port->place-message port)))])

;; ----------------------------------------

;; in atomic mode or with custodian lock
;; current custodian must not be shut down
(define (open-output-fd fd name
                        #:is-terminal? is-terminal?
                        #:buffer-mode [buffer-mode 'infer]
                        #:fd-refcount [fd-refcount (box 1)]
                        #:plumber [plumber (current-plumber)]
                        #:custodian [cust (current-custodian)])
  (finish-fd-output-port
   (new fd-output-port
        #:field
        [name name]
        [fd fd]
        [fd-refcount fd-refcount]
        [buffer-mode
         (if (eq? buffer-mode 'infer)
             (if is-terminal?
                 'line
                 'block)
             buffer-mode)])
   #:plumber plumber
   #:custodian cust))

;; in atomic mode or with custodian lock
;; current custodian must not be shut down
(define (finish-fd-output-port p
                               #:plumber [plumber (current-plumber)]
                               #:custodian [cust (current-custodian)])
  (define fd (fd-output-port-fd p))
  (define fd-refcount (fd-output-port-fd-refcount p))
  (define evt (fd-evt fd RKTIO_POLL_WRITE fd-refcount))
  (define flush-handle (and plumber
                            (plumber-add-flush! plumber
                                                (lambda (h)
                                                  (with-lock p
                                                   (send fd-output-port p flush-buffer/external))))))
  (define custodian-reference (register-fd-close cust fd fd-refcount flush-handle p))
  (set-core-output-port-evt! p evt)
  (set-fd-output-port-flush-handle! p flush-handle)
  (set-fd-output-port-custodian-reference! p custodian-reference)
  (finish-port/count p))

;; ----------------------------------------

(define (terminal-port? p)
  (define cp (or (->core-input-port p #:default #f)
                 (->core-output-port p #:default #f)))
  (cond
    [(not cp) #f]
    [else
     (with-lock cp
       (define fd (fd-port-fd cp))
       (and fd
            (rktioly (rktio_fd_is_terminal rktio fd))))]))

;; with lock held or in atomic mode, the latter when the port's lock has been forced to be atomic
(define (fd-port-fd cp)
  (cond
    [(fd-input-port? cp)
     (fd-input-port-fd cp)]
    [(fd-output-port? cp)
     (fd-output-port-fd cp)]
    [else #f]))

(define/who (port-waiting-peer? p)
  (define cp (->core-output-port p #:default #f))
  (cond
    [cp
     (cond
       [(fd-output-port? cp)
        (with-lock cp
          (define fd (fd-port-fd cp))
          (rktioly (rktio_fd_is_pending_open rktio fd)))]
       [else #f])]
    [(input-port? p) #f]
    [else
     (raise-argument-error who "port?" p)]))

;; ----------------------------------------

;; lock held and *not* in rktio mode
(define (get-file-position fd)
  (rktioly
   (define ppos (rktio_get_file_position rktio fd))
   (cond
     [(rktio-error? ppos)
      ;; #f => not supported, so use port's own counter, instead
      #f]
     [else
      (define pos (rktio_filesize_ref ppos))
      (rktio_free ppos)
      pos])))

;; lock held for p and *not* in rktio mode
(define (set-file-position fd pos p)
  (define r
    (rktioly
     (rktio_set_file_position rktio
                              fd
                              (if (eof-object? pos)
                                  0
                                  pos)
                              (if (eof-object? pos)
                                  RKTIO_POSITION_FROM_END
                                  RKTIO_POSITION_FROM_START))))
  (when (rktio-error? r)
    (port-unlock p)
    (raise-rktio-error 'file-position r "error setting stream position")))

;; ----------------------------------------

;; The ready value for an `fd-evt` is 0, so it can be used directly
;; for an input port

(struct fd-evt (fd mode fd-refcount)
  #:property
  prop:evt
  (poller
   ;; This function is called by the scheduler for `sync` to check
   ;; whether the file descriptor has data available:
   (lambda (fde ctx)
     (cond
       [(zero? (unbox (fd-evt-fd-refcount fde)))
        (values '(0) #f)]
       [else
        (define mode (fd-evt-mode fde))
        (define ready?
          (or
           (and (eqv? RKTIO_POLL_READ (bitwise-and mode RKTIO_POLL_READ))
                (eqv? (rktio_poll_read_ready rktio (fd-evt-fd fde))
                      RKTIO_POLL_READY))
           (and (eqv? RKTIO_POLL_WRITE (bitwise-and mode RKTIO_POLL_WRITE))
                (eqv? (rktio_poll_write_ready rktio (fd-evt-fd fde))
                      RKTIO_POLL_READY))))
        (cond
          [ready?
           (values '(0) #f)]
          ;; If the called is going to block (i.e., not just polling), then
          ;; try to get a semaphore to represent the file descriptor, because
          ;; that can be more scalable (especially for lots of TCP sockets)
          [(and (not (sandman-poll-ctx-poll? ctx))
                (rktioly
                 (fd-semaphore-update! (fd-evt-fd fde)
                                       (if (eqv? RKTIO_POLL_READ (bitwise-and mode RKTIO_POLL_READ))
                                           'read
                                           'write))))
           => (lambda (s) ; got a semaphore
                (values #f (wrap-evt s (lambda (s) 0))))]
          [else
           ;; If `sched-info` in `poll-ctx` is not #f, then we can register this file
           ;; descriptor so that if no thread is able to make progress,
           ;; the Racket process will sleep, but it will wake up when
           ;; input is available. The implementation of external events
           ;; is from the current sandman, which will in turn be the
           ;; one (or build on the one) in "../sandman".
           (sandman-poll-ctx-add-poll-set-adder!
            ctx
            ;; Cooperate with the sandman by registering
            ;; a function that takes a poll set and
            ;; adds to it:
            ;; in atomic and in rktio-sleep-relevant (not rktio), must not start nested rktio
            (lambda (ps)
              (if (zero? (unbox (fd-evt-fd-refcount fde)))
                  (rktio_poll_set_add_nosleep rktio ps)
                  (rktio_poll_add rktio (fd-evt-fd fde) ps mode))))
           (values #f fde)])]))))

;; ----------------------------------------
;; Wait on rktio-level flushing. At the time of writing, this is
;; needed only for Windows so old that Racket CS doesn't run on it,
;; but here just in case rktio or something else changes.

(struct rktio-fd-flushed-evt (p)
  #:property
  prop:evt
  (poller
   (lambda (ffe ctx)
     (define p  (rktio-fd-flushed-evt-p ffe))
     (cond
       [(rktioly (send fd-output-port p rktio-flushed?))
        (values '(#t) #f)]
       [else
        (sandman-poll-ctx-add-poll-set-adder!
         ctx
         ;; in atomic and in rktio, must not start nested rktio
         (lambda (ps)
           (if (send fd-output-port p rktio-flushed?)
               (rktio_poll_set_add_nosleep rktio ps)
               (rktio_poll_add rktio (fd-output-port-fd p) ps RKTIO_POLL_FLUSH))))
        (values #f (list ffe))]))))

;; ----------------------------------------

(define (register-fd-close custodian fd fd-refcount flush-handle port)
  (unsafe-custodian-register custodian
                             port
                             ;; in atomic mode
                             (lambda (port)
                               (when flush-handle
                                 (plumber-flush-handle-remove! flush-handle))
                               (with-lock port
                                 (rktioly
                                  (start-rktio-sleep-relevant)
                                  (if (input-port? port)
                                      (send fd-input-port port on-close)
                                      (send fd-output-port port on-close))
                                  (fd-close fd fd-refcount port #:discard-errors? #t)
                                  (set-closed-state! port)
                                  (end-rktio-sleep-relevant))))
                             #f
                             #f))

;; ----------------------------------------

(define-values (prop:fd-place-message-opener fd-place-message-opener? fd-place-message-opener-ref)
  (make-struct-type-property 'fd-place-message-opener))

(define (fd-port->place-message port)
  (port-lock port)
  (cond
    [(port-closed? port)
     (port-unlock port)
     #f]
    [else
     (define input? (input-port? port))
     (define fd-dup (dup-port-fd port))
     (define name (core-port-name port))
     (define is-terminal? (and (not input?)
                               (rktioly (rktio_fd_is_terminal rktio (unbox fd-dup)))))
     (define opener (or (fd-place-message-opener-ref port #f)
                        (if input?
                            (lambda (fd name) (open-input-fd fd name))
                            (lambda (fd name) (open-output-fd fd name #:is-terminal? is-terminal?)))))
     (port-unlock port)
     (lambda ()
       (atomically
        (define fd (claim-dup fd-dup))
        (opener fd name)))]))

;; with lock held and in atomic mode
(define (dup-port-fd port)
  (define fd (fd-port-fd port))
  (start-rktio)
  (define new-fd (rktio_dup rktio fd))
  (when (rktio-error? new-fd)
    (end-rktio)
    (port-unlock port)
    (raise-rktio-error 'place-channel-put new-fd "error during dup of file descriptor"))
  (define fd-dup (box (rktio_fd_detach rktio new-fd)))
  (end-rktio)
  (unsafe-add-global-finalizer fd-dup (lambda ()
                                        (define fd (unbox fd-dup))
                                        (when fd
                                          ;; no rktio argument, so no start-rktio, which
                                          ;; is important for something like the argument
                                          ;; to `unsafe-add-global-finalizer` that can run
                                          ;; as part of the garbage-collection callbacks
                                          (rktio_fd_close_transfer fd))))
  fd-dup)

;; in atomic mode
(define (claim-dup fd-dup)
  (define fd (unbox fd-dup))
  (set-box! fd-dup #f)
  (rktioly (rktio_fd_attach rktio fd)))
