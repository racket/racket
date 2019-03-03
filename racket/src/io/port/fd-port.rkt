#lang racket/base
(require racket/fixnum
         "../common/class.rkt"
         "../host/rktio.rkt"
         "../host/error.rkt"
         "../host/thread.rkt"
         "../host/pthread.rkt"
         "../sandman/main.rkt"
         "../file/error.rkt"
         "port.rkt"
         "input-port.rkt"
         "output-port.rkt"
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
         fd-port-fd
         prop:fd-place-message-opener)

;; in atomic mode
(define (fd-close fd fd-refcount)
  (set-box! fd-refcount (sub1 (unbox fd-refcount)))
  (when (zero? (unbox fd-refcount))
    (define v (rktio_close rktio fd))
    (when (rktio-error? v)
      (end-atomic)
      (raise-rktio-error #f v "error closing stream port"))))

;; ----------------------------------------

(class fd-input-port #:extends peek-via-read-input-port
  #:field
  [fd #f]
  [fd-refcount (box 1)]
  [custodian-reference #f]
  
  #:public
  [on-close (lambda () (void))]
  [raise-read-error (lambda (n)
                      (raise-filesystem-error #f n "error reading from stream port"))]
  
  #:override
  [read-in/inner
   (lambda (dest-bstr start end copy?)
     (define n (rktio_read_in rktio fd dest-bstr start end))
     (cond
       [(rktio-error? n)
        (end-atomic)
        (send fd-input-port this raise-read-error n)]
       [(eqv? n RKTIO_READ_EOF) eof]
       [(eqv? n 0) (wrap-evt (fd-evt fd RKTIO_POLL_READ this)
                             (lambda (v) 0))]
       [else n]))]

  [close
   (lambda ()
     (send fd-input-port this on-close)
     (fd-close fd fd-refcount)
     (unsafe-custodian-unregister fd custodian-reference)
     (close-peek-buffer))]

  [file-position
   (case-lambda
     [()
      (define pos (get-file-position fd))
      (and pos (buffer-adjust-pos pos))]
     [(pos)
      (purge-buffer)
      (set-file-position fd pos)])]

  #:property
  [prop:file-stream (lambda (p) (fd-input-port-fd p))]
  [prop:place-message (lambda (port)
                        (lambda ()
                          (fd-port->place-message port)))])

;; ----------------------------------------

;; in atomic mode
;; Current custodian must not be shut down.
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
  [fd-refcount (box 1)]
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
  ;; in atomic mode
  ;; Returns `#t` if the buffer is already or successfully flushed
  [flush-buffer
   (lambda ()
     (slow-mode!)
     (cond
       [(not (fx= start-pos end-pos))
        (define n (rktio_write_in rktio fd bstr start-pos end-pos))
        (cond
          [(rktio-error? n)
           (end-atomic)
           (send fd-output-port this raise-write-error n)]
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

  ;; in atomic mode, but may leave it temporarily
  [flush-buffer-fully
   (lambda (enable-break?)
     (let loop ()
       (unless (flush-buffer)
         (end-atomic)
         (if enable-break?
             (sync/enable-break evt)
             (sync evt))
         (start-atomic)
         (when bstr ; in case it was closed
           (loop)))))]

  ;; in atomic mode, but may leave it temporarily
  [flush-buffer-fully-if-newline
   (lambda (src-bstr src-start src-end enable-break?)
     (for ([b (in-bytes src-bstr src-start src-end)])
       (define newline? (or (eqv? b (char->integer #\newline))
                            (eqv? b (char->integer #\return))))
       (when newline? (flush-buffer-fully enable-break?))
       #:break newline?
       (void)))]

  #:static
  [flush-buffer/external
   (lambda ()
     (flush-buffer-fully #f))]

  #:override
  ;; in atomic mode
  [write-out
   (lambda (src-bstr src-start src-end nonbuffer/nonblock? enable-break? copy?)
     (slow-mode!)
     (cond
       [(fx= src-start src-end)
        ;; Flush request
        (and (flush-buffer) 0)]
       [(and (not (eq? buffer-mode 'none))
             (not nonbuffer/nonblock?)
             (fx< end-pos (bytes-length bstr)))
        (define amt (fxmin (fx- src-end src-start) (fx- (bytes-length bstr) end-pos)))
        (bytes-copy! bstr end-pos src-bstr src-start (fx+ src-start amt))
        (set! end-pos (fx+ end-pos amt))
        (when (eq? buffer-mode 'line)
          ;; can temporarily leave atomic mode:
          (flush-buffer-fully-if-newline src-bstr src-start src-end enable-break?))
        (fast-mode! amt)
        amt]
       [(not (flush-buffer)) ; <- can temporarily leave atomic mode
        #f]
       [else
        (define n (rktio_write_in rktio fd src-bstr src-start src-end))
        (cond
          [(rktio-error? n)
           (end-atomic)
           (send fd-output-port this raise-write-error n)]
          [(fx= n 0) (wrap-evt evt (lambda (v) #f))]
          [else n])]))]

  [get-write-evt
   (get-write-evt-via-write-out (lambda (out v bstr start)
                                  (port-count! out v bstr start)))]

  ;; in atomic mode
  [close
   (lambda ()
     (flush-buffer-fully #f) ; can temporarily leave atomic mode
     (when bstr ; <- in case a concurrent close succeeded
       (send fd-output-port this on-close)
       (plumber-flush-handle-remove! flush-handle)
       (set! bstr #f)
       (fd-close fd fd-refcount)
       (unsafe-custodian-unregister fd custodian-reference)))]

  ;; in atomic mode
  [file-position
   (case-lambda
     [()
      (define pos (get-file-position fd))
      (define b buffer)
      (and pos (+ pos (fx- (if (direct-bstr b) (direct-pos b) end-pos) start-pos)))]
     [(pos)
      (flush-buffer-fully #f)
      ;; flushing can leave atomic mode, so make sure the
      ;; port is still open before continuing
      (unless bstr
        (check-not-closed 'file-position this))
      (set-file-position fd pos)])]

  ;; in atomic mode
  [buffer-mode
   (case-lambda
     [() buffer-mode]
     [(mode) (set! buffer-mode mode)])]

  #:property
  [prop:file-stream (lambda (p) (fd-output-port-fd p))]
  [prop:file-truncate (lambda (p pos)
                        ;; in atomic mode
                        (send fd-output-port p flush-buffer/external)
                        (define result
                          (rktio_set_file_size rktio
                                               (fd-output-port-fd p)
                                               pos))
                        (when (rktio-error? result)
                          (end-atomic)
                          (raise-rktio-error 'file-truncate result  "error setting file size")))]
  [prop:place-message (lambda (port)
                        (lambda ()
                          (fd-port->place-message port)))])

;; ----------------------------------------

;; in atomic mode
;; Current custodian must not be shut down.
(define (open-output-fd fd name
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
             (if (rktio_fd_is_terminal rktio fd)
                 'line
                 'block)
             buffer-mode)])
   #:plumber plumber
   #:custodian cust))

(define (finish-fd-output-port p
                               #:plumber [plumber (current-plumber)]
                               #:custodian [cust (current-custodian)])
  (define fd (fd-output-port-fd p))
  (define fd-refcount (fd-output-port-fd-refcount p))
  (define evt (fd-evt fd RKTIO_POLL_WRITE p))
  (define flush-handle (plumber-add-flush! plumber
                                           (lambda (h)
                                             (atomically
                                              (send fd-output-port p flush-buffer/external)))))
  (define custodian-reference (register-fd-close cust fd fd-refcount flush-handle p))
  (set-core-output-port-evt! p evt)
  (set-fd-output-port-flush-handle! p flush-handle)
  (set-fd-output-port-custodian-reference! p custodian-reference)
  (finish-port/count p))

;; ----------------------------------------

(define (terminal-port? p)
  (define fd (fd-port-fd p))
  (and fd
       (rktio_fd_is_terminal rktio fd)))

(define (fd-port-fd p)
  (define cp (or (->core-input-port p #:default #f)
                 (->core-output-port p #:default #f)))
  (cond
    [(fd-input-port? cp)
     (fd-input-port-fd cp)]
    [(fd-output-port? cp)
     (fd-output-port-fd cp)]
    [else #f]))

;; ----------------------------------------

;; in atomic mode
(define (get-file-position fd)
  (define ppos (rktio_get_file_position rktio fd))
  (cond
    [(rktio-error? ppos)
     ;; #f => not supported, so use port's own counter, instead
     #f]
    [else
     (define pos (rktio_filesize_ref ppos))
     (rktio_free ppos)
     pos]))

;; in atomic mode
(define (set-file-position fd pos)
  (define r
    (rktio_set_file_position rktio
                             fd
                             (if (eof-object? pos)
                                 0
                                 pos)
                             (if (eof-object? pos)
                                 RKTIO_POSITION_FROM_END
                                 RKTIO_POSITION_FROM_START)))
  (when (rktio-error? r)
    (end-atomic)
    (raise-rktio-error 'file-position r "error setting stream position")))

;; ----------------------------------------

(struct fd-evt (fd mode [closed #:mutable])
  #:property
  prop:evt
  (poller
   ;; This function is called by the scheduler for `sync` to check
   ;; whether the file descriptor has data available:
   (lambda (fde ctx)
     (cond
       [(core-port-closed? (fd-evt-closed fde))
        (values (list fde) #f)]
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
           (values (list fde) #f)]
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
            (lambda (ps)
              (rktio_poll_add rktio (fd-evt-fd fde) ps mode)))
           (values #f fde)])]))))

;; ----------------------------------------

(define (register-fd-close custodian fd fd-refcount flush-handle port)
  (unsafe-custodian-register custodian
                             fd
                             ;; in atomic mode
                             (lambda (fd)
                               (when flush-handle
                                 (plumber-flush-handle-remove! flush-handle))
                               (fd-close fd fd-refcount)
                               (set-closed-state! port))
                             #f
                             #f))

;; ----------------------------------------

(define-values (prop:fd-place-message-opener fd-place-message-opener? fd-place-message-opener-ref)
  (make-struct-type-property 'fd-place-message-opener))

(define (fd-port->place-message port)
  (start-atomic)
  (cond
    [(port-closed? port) #f]
    [else
     (define input? (input-port? port))
     (define fd-dup (dup-port-fd port))
     (define name (core-port-name port))
     (define opener (or (fd-place-message-opener-ref port #f)
                        (if input?
                            (lambda (port name) (open-input-fd port name))
                            (lambda (port name) (open-output-fd port name)))))
     (end-atomic)
     (lambda ()
       (atomically
        (define fd (claim-dup fd-dup))
        (opener fd name)))]))

  ;; in atomic mode
(define (dup-port-fd port)
  (define fd (fd-port-fd port))
  (define new-fd (rktio_dup rktio fd))
  (when (rktio-error? new-fd)
    (end-atomic)
    (raise-rktio-error 'place-channel-put new-fd "error during dup of file descriptor"))
  (define fd-dup (box (rktio_fd_detach rktio new-fd)))
  (unsafe-add-global-finalizer fd-dup (lambda ()
                                        (define fd (unbox fd-dup))
                                        (when fd
                                          (rktio_fd_close_transfer fd))))
  fd-dup)

;; in atomic mode
(define (claim-dup fd-dup)
  (define fd (unbox fd-dup))
  (set-box! fd-dup #f)
  (rktio_fd_attach rktio fd))
