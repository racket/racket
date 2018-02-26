#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "port.rkt"
         "evt.rkt")

(provide prop:input-port
         input-port?
         ->core-input-port
         (struct-out core-input-port)
         make-core-input-port)

(define-values (prop:input-port input-port-via-property? input-port-ref)
  (make-struct-type-property 'input-port
                             (lambda (v sti)
                               (check 'prop:input-port (lambda (v) (or (exact-nonnegative-integer? v)
                                                                       (input-port? v)))
                                      #:contract "(or/c input-port? exact-nonnegative-integer?)"
                                      v)
                               (check-immutable-field 'prop:input-port v sti)
                               (if (exact-nonnegative-integer? v)
                                   (make-struct-field-accessor (list-ref sti 3) v)
                                   v))
                             (list (cons prop:secondary-evt
                                         (lambda (v) port->evt))
                                   (cons prop:input-port-evt
                                         (lambda (i)
                                           (input-port-evt-ref (->core-input-port i)))))))

(define (input-port? p)
  (or (core-input-port? p)
      (input-port-via-property? p)))

;; This function should not be called in atomic mode,
;; since it can invoke an artitrary function
(define (->core-input-port v)
  (cond
    [(core-input-port? v) v]
    [(input-port? v)
     (let ([p (input-port-ref v)])
       (cond
         [(struct-accessor-procedure? p)
          (->core-input-port (p v))]
         [else
          (->core-input-port p)]))]
    [else
     empty-input-port]))

(struct core-input-port core-port
  (
   ;; Various functions below are called in atomic mode. The intent of
   ;; atomic mode is to ensure that the completion and return of the
   ;; function is atomic with respect to some further activity, such
   ;; as position and line counting. Also, a guard against operations
   ;; on a closed port precedes most operations. Any of the functions
   ;; is free to exit and re-enter atomic mode, but they may take on
   ;; the burden of re-checking for a closed port. Leave atomic mode
   ;; explicitly before raising an exception.

   prepare-change ; #f or (-> void)
   ;;               Called in atomic mode
   ;;               May leave atomic mode temporarily, but on return,
   ;;               ensures that other atomic operations are ok to
   ;;               change the port. The main use of `prepare-change`
   ;;               is to pause and `port-commit-peeked` attempts to
   ;;               not succeed while a potential change is in
   ;;               progress, where the commit attempts can resume after
   ;;               atomic mode is left. The `close` operation
   ;;               is *not* guarded by a call to `prepare-change`.

   read-byte ; #f or (-> (or/c byte? eof-object? evt?))
   ;;          Called in atomic mode.
   ;;          This shortcut is optional.
   ;;          Non-blocking byte read, where an event must be
   ;;          returned if no byte is available. The event's result
   ;;          is ignored, so it should not consume a byte.

   read-in   ; port or (bytes start-k end-k copy? -> (or/c integer? ...))
   ;;          Called in atomic mode.
   ;;          A port value redirects to the port. Otherwise, the function
   ;;          never blocks, and can assume `(- end-k start-k)` is non-zero.
   ;;          The `copy?` flag indicates that the given byte string should
   ;;          not be exposed to untrusted code, and instead of should be
   ;;          copied if necessary. The return values are the same as
   ;;          documented for `make-input-port`, except that a pipe result
   ;;          is not allowed (or, more precisely, it's treated as an event).

   peek-byte ; #f or (-> (or/c byte? eof-object? evt?))
   ;;          Called in atomic mode.
   ;;          This shortcut is optional.
   ;;          Non-blocking byte read, where an event must be
   ;;          returned if no byte is available. The event's result
   ;;          is ignored.

   peek-in   ; port or (bytes start-k end-k skip-k progress-evt copy? -> (or/c integer? ...))
   ;;          Called in atomic mode.
   ;;          A port value redirects to the port. Otherwise, the function
   ;;          never blocks, and it can assume that `(- end-k start-k)` is non-zero.
   ;;          The `copy?` flag is the same as for `read-in`.  The return values
   ;;          are the same as documented for `make-input-port`.

   byte-ready  ; port or ((->) -> (or/c boolean? evt))
   ;;          Called in atomic mode.
   ;;          A port value makes sense when `peek-in` has a port value.
   ;;          Otherwise, check whether a peek on one byte would succeed
   ;;          without blocking and return a boolean, or return an event
   ;;          that effectively does the same. The event's value doesn't
   ;;          matter, because it will be wrapped to return some original
   ;;          port. When `byte-ready` is a function, it should call the
   ;;          given funciton (for its side effect) when work has been
   ;;          done that might unblock this port or some other port.

   get-progress-evt ; #f or (-> evt?)
   ;;           *Not* called in atomic mode.
   ;;           Optional support for progress events, and may be
   ;;           called on a closed port.

   commit    ; (amt-k progress-evt? evt? (bytes? -> any) -> boolean)
   ;;          Called in atomic mode.
   ;;          Goes with `get-progress-evt`. The final `evt?`
   ;;          argument is constrained to a few kinds of events;
   ;;          see docs for `port-commit-peeked` for more information.
   ;;          On success, a completion function is called in atomic mode,
   ;;          but possibly in a different thread, with the committed bytes.
   ;;          The result is a boolean indicating success or failure.

   [pending-eof? #:mutable]
   [read-handler #:mutable])
  #:authentic
  #:property prop:input-port-evt (lambda (i)
                                   (cond
                                     [(closed-state-closed? (core-port-closed i))
                                      always-evt]
                                     [else
                                      (define byte-ready (core-input-port-byte-ready i))
                                      (cond
                                        [(input-port? byte-ready)
                                         byte-ready]
                                        [else
                                         (poller-evt
                                          (poller
                                           (lambda (self poll-ctx)
                                             (define v (byte-ready (lambda ()
                                                                     (schedule-info-did-work! (poll-ctx-sched-info poll-ctx)))))
                                             (cond
                                               [(evt? v)
                                                (values #f v)]
                                               [(eq? v #t)
                                                (values (list #t) #f)]
                                               [else
                                                (values #f self)]))))])])))

(define (make-core-input-port #:name name
                              #:data [data #f]
                              #:prepare-change [prepare-change #f]
                              #:read-byte [read-byte #f]
                              #:read-in read-in
                              #:peek-byte [peek-byte #f]
                              #:peek-in peek-in
                              #:byte-ready byte-ready
                              #:close close
                              #:get-progress-evt [get-progress-evt #f]
                              #:commit [commit #f]
                              #:get-location [get-location #f]
                              #:count-lines! [count-lines! #f]
                              #:init-offset [init-offset 0]
                              #:file-position [file-position #f]
                              #:buffer-mode [buffer-mode #f])
  (core-input-port name
                   data

                   close
                   count-lines!
                   get-location
                   file-position
                   buffer-mode
                   
                   (closed-state #f #f)
                   init-offset ; offset
                   #f   ; count?
                   #f   ; state
                   #f   ; cr-state
                   #f   ; line
                   #f   ; column
                   #f   ; position

                   prepare-change
                   read-byte
                   read-in
                   peek-byte
                   peek-in
                   byte-ready
                   get-progress-evt
                   commit
                   #f   ; pending-eof?
                   #f)) ; read-handler

(define empty-input-port
  (make-core-input-port #:name 'empty
                        #:read-in (lambda (bstr start-k end-k copy?) eof)
                        #:peek-in (lambda (bstr start-k end-k skip-k copy?) eof)
                        #:byte-ready (lambda (did-work!) #f)
                        #:close void))
