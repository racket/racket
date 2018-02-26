#lang racket/base
(require "../host/rktio.rkt"
         "../host/error.rkt"
         "../host/thread.rkt"
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

(provide open-input-fd
         open-output-fd
         terminal-port?
         fd-port-fd
         maybe-fd-data-extra)

(struct fd-data (fd extra)
  #:property prop:file-stream (lambda (fdd) (fd-data-fd fdd))
  #:property prop:file-truncate (case-lambda
                                  [(fdd pos)
                                   (check-rktio-error*
                                    (rktio_set_file_size rktio
                                                         (fd-data-fd fdd)
                                                         pos)
                                    "error setting file size")]))

(define (maybe-fd-data-extra data)
  (and (fd-data? data)
       (fd-data-extra data)))

;; in atomic mode
(define (fd-close fd fd-refcount)
  (set-box! fd-refcount (sub1 (unbox fd-refcount)))
  (when (zero? (unbox fd-refcount))
    (define v (rktio_close rktio fd))
    (when (rktio-error? v)
      (end-atomic)
      (raise-rktio-error #f v "error closing stream port"))))

;; ----------------------------------------

;; in atomic mode
;; Current custodian must not be shut down.
(define (open-input-fd fd name
                       #:extra-data [extra-data #f]
                       #:on-close [on-close void]
                       #:fd-refcount [fd-refcount (box 1)])
  (define-values (port buffer-control)
    (open-input-peek-via-read
     #:name name
     #:data (fd-data fd extra-data)
     #:read-in
     ;; in atomic mode
     (lambda (dest-bstr start end copy?)
       (define n (rktio_read_in rktio fd dest-bstr start end))
       (cond
         [(rktio-error? n)
          (end-atomic)
          (raise-filesystem-error #f n "error reading from stream port")]
         [(eqv? n RKTIO_READ_EOF) eof]
         [(eqv? n 0) (wrap-evt (fd-evt fd RKTIO_POLL_READ (core-port-closed port))
                               (lambda (v) 0))]
         [else n]))
     #:read-is-atomic? #t
     #:close
     ;; in atomic mode
     (lambda ()
       (on-close)
       (fd-close fd fd-refcount)
       (unsafe-custodian-unregister fd custodian-reference))
     #:file-position (make-file-position
                      fd
                      (case-lambda
                        [() (buffer-control)]
                        [(pos) (buffer-control pos)]))))
  (define custodian-reference
    (register-fd-close (current-custodian) fd fd-refcount port))
  port)

;; ----------------------------------------

;; in atomic mode
;; Current custodian must not be shut down.
(define (open-output-fd fd name
                        #:extra-data [extra-data #f]
                        #:buffer-mode [buffer-mode 'infer]
                        #:fd-refcount [fd-refcount (box 1)]
                        #:on-close [on-close void])
  (define buffer (make-bytes 4096))
  (define buffer-start 0)
  (define buffer-end 0)
  (define flush-handle
    (plumber-add-flush! (current-plumber)
                        (lambda (h)
                          (flush-buffer-fully #f)
                          (plumber-flush-handle-remove! h))))
  
  (when (eq? buffer-mode 'infer)
    (if (rktio_fd_is_terminal rktio fd)
        (set! buffer-mode 'line)
        (set! buffer-mode 'block)))

  (define evt (fd-evt fd RKTIO_POLL_WRITE #f))

  ;; in atomic mode
  ;; Returns `#t` if the buffer is already or successfully flushed
  (define (flush-buffer)
    (cond
      [(not (= buffer-start buffer-end))
       (define n (rktio_write_in rktio fd buffer buffer-start buffer-end))
       (cond
         [(rktio-error? n)
          (end-atomic)
          (raise-filesystem-error #f n "error writing to stream port")]
         [(zero? n)
          #f]
         [else
          (define new-buffer-start (+ buffer-start n))
          (cond
            [(= new-buffer-start buffer-end)
             (set! buffer-start 0)
             (set! buffer-end 0)
             #t]
            [else
             (set! buffer-start new-buffer-start)
             #f])])]
      [else #t]))

  ;; in atomic mode
  (define (flush-buffer-fully enable-break?)
    (let loop ()
      (unless (flush-buffer)
        (end-atomic)
        (if enable-break?
            (sync/enable-break evt)
            (sync evt))
        (start-atomic)
        (when buffer ; in case it was closed
          (loop)))))

  ;; in atomic mode
  (define (flush-buffer-fully-if-newline src-bstr src-start src-end enable-break?)
    (for ([b (in-bytes src-bstr src-start src-end)])
      (define newline? (or (eqv? b (char->integer #\newline))
                           (eqv? b (char->integer #\return))))
      (when newline? (flush-buffer-fully enable-break?))
      #:break newline?
      (void)))

  (define port
    (make-core-output-port
     #:name name
     #:data (fd-data fd extra-data)

     #:evt evt
     
     #:write-out
     ;; in atomic mode
     (lambda (src-bstr src-start src-end nonbuffer/nonblock? enable-break? copy?)
       (cond
         [(= src-start src-end)
          ;; Flush request
          (and (flush-buffer) 0)]
         [(and (not (eq? buffer-mode 'none))
               (not nonbuffer/nonblock?)
               (< buffer-end (bytes-length buffer)))
          (define amt (min (- src-end src-start) (- (bytes-length buffer) buffer-end)))
          (bytes-copy! buffer buffer-end src-bstr src-start (+ src-start amt))
          (set! buffer-end (+ buffer-end amt))
          (unless nonbuffer/nonblock?
            (when (eq? buffer-mode 'line)
              ;; can temporarily leave atomic mode:
              (flush-buffer-fully-if-newline src-bstr src-start src-end enable-break?)))
          amt]
         [(not (flush-buffer)) ; <- can temporarily leave atomic mode
          #f]
         [else
          (define n (rktio_write_in rktio fd src-bstr src-start src-end))
          (cond
            [(rktio-error? n)
             (end-atomic)
             (raise-filesystem-error #f n "error writing to stream port")]
            [(zero? n) (wrap-evt evt (lambda (v) #f))]
            [else n])]))

     #:count-write-evt-via-write-out
     (lambda (v bstr start)
       (port-count! port v bstr start))

     #:close
     ;; in atomic mode
     (lambda ()
       (flush-buffer-fully #f) ; can temporarily leave atomic mode
       (when buffer ; <- in case a concurrent close succeeded
         (on-close)
         (plumber-flush-handle-remove! flush-handle)
         (set! buffer #f)
         (fd-close fd fd-refcount)
         (unsafe-custodian-unregister fd custodian-reference)))

     #:file-position (make-file-position
                      fd
                      ;; in atomic mode
                      (case-lambda
                        [()
                         (flush-buffer-fully #f)
                         ;; flushing can leave atomic mode, so make sure the
                         ;; port is still open before continuing
                         (unless buffer
                           (check-not-closed 'file-position port))]
                        [(pos)
                         (+ pos (- buffer-end buffer-start))]))
     #:buffer-mode (case-lambda
                     [() buffer-mode]
                     [(mode) (set! buffer-mode mode)])))

  (define custodian-reference
    (register-fd-close (current-custodian) fd fd-refcount port))

  (set-fd-evt-closed! evt (core-port-closed port))

  port)

;; ----------------------------------------

(define (terminal-port? p)
  (define data
    (core-port-data
     (cond
       [(input-port? p) (->core-input-port p)]
       [(output-port? p) (->core-output-port p)]
       [else
        (raise-argument-error 'terminal-port? "port?" p)])))
  (and (fd-data? data)
       (rktio_fd_is_terminal rktio (fd-data-fd data))))

(define (fd-port-fd p)
  (define data
    (core-port-data
     (cond
       [(input-port? p) (->core-input-port p)]
       [else (->core-output-port p)])))
  (and (fd-data? data)
       (fd-data-fd data)))

;; ----------------------------------------

(define (make-file-position fd buffer-control)
  ;; in atomic mode
  (case-lambda
    [()
     (define ppos (rktio_get_file_position rktio fd))
     (cond
       [(rktio-error? ppos)
        ;; #f => not supported, so use port's own counter, instead
        #f]
       [else
        (define pos (rktio_filesize_ref ppos))
        (rktio_free ppos)
        (buffer-control pos)])]
    [(pos)
     (buffer-control)
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
       (raise-rktio-error 'file-position r "error setting stream position"))]))

;; ----------------------------------------

(struct fd-evt (fd mode [closed #:mutable])
  #:property
  prop:evt
  (poller
   ;; This function is called by the scheduler for `sync` to check
   ;; whether the file descriptor has data available:
   (lambda (fde ctx)
     (cond
       [(closed-state-closed? (fd-evt-closed fde))
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

(define (register-fd-close custodian fd fd-refcount port)
  (define closed (core-port-closed port))
  (unsafe-custodian-register custodian
                             fd
                             ;; in atomic mode
                             (lambda (fd)
                               (fd-close fd fd-refcount)
                               (set-closed-state! closed))
                             #f
                             #f))
