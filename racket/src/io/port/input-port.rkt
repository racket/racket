#lang racket/base
(require "../common/check.rkt"
         "../common/class.rkt"
         "../host/thread.rkt"
         "../host/pthread.rkt"
         "port.rkt"
         "evt.rkt")

(provide prop:input-port
         input-port?
         ->core-input-port
         (struct-out core-input-port)
         make-core-input-port
         compat-input-port-self)

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
(define (->core-input-port v [who #f])
  (cond
    [(core-input-port? v) v]
    [(input-port-ref v #f)
     => (lambda (p)
          (cond
            [(struct-accessor-procedure? p)
             (->core-input-port (p v))]
            [else
             (->core-input-port p)]))]
    [who (raise-argument-error who "input-port?" v)]
    [else empty-input-port]))

(class core-input-port #:extends core-port
  (field
   [pending-eof? #f]
   [read-handler #f])

  (public

   ;; #f or (-*> void)
   ;; Called in atomic mode
   ;; May leave atomic mode temporarily, but on return, ensures that
   ;; other atomic operations are ok to change the port. The main use
   ;; of `prepare-change` is to pause and `port-commit-peeked`
   ;; attempts to not succeed while a potential change is in progress,
   ;; where the commit attempts can resume after atomic mode is left.
   ;; The `close` operation is *not* guarded by a call to
   ;; `prepare-change`.
   [prepare-change #f]

   ;; port or (bytes start-k end-k copy? -*> (or/c integer? ...))
   ;; Called in atomic mode.
   ;; A port value redirects to the port. Otherwise, the function
   ;; never blocks, and can assume `(- end-k start-k)` is non-zero.
   ;; The `copy?` flag indicates that the given byte string should not
   ;; be exposed to untrusted code, and instead of should be copied if
   ;; necessary. The return values are the same as documented for
   ;; `make-input-port`, except that a pipe result is not allowed (or,
   ;; more precisely, it's treated as an event).
   [read-in (lambda (bstr start end copy?) eof)]

   ;; port or (bytes start-k end-k skip-k progress-evt copy? -*> (or/c integer? ...))
   ;; Called in atomic mode.
   ;; A port value redirects to the port. Otherwise, the function
   ;; never blocks, and it can assume that `(- end-k start-k)` is
   ;; non-zero. The `copy?` flag is the same as for `read-in`. The
   ;; return values are the same as documented for `make-input-port`.
   [peek-in (lambda (bstr start end progress-evt copy?) eof)]

   ;; port or ((->) -*> (or/c boolean? evt))
   ;; Called in atomic mode.
   ;; A port value makes sense when `peek-in` has a port value.
   ;; Otherwise, check whether a peek on one byte would succeed
   ;; without blocking and return a boolean, or return an event that
   ;; effectively does the same. The event's value doesn't matter,
   ;; because it will be wrapped to return some original port. When
   ;; `byte-ready` is a function, it should call the given function
   ;; (for its side effect) when work has been done that might unblock
   ;; this port or some other port.
   [byte-ready (lambda (work-done!) #t)]

   ;; #f or (-*> evt?)
   ;; *Not* called in atomic mode.
   ;; Optional support for progress events, and may be called on a
   ;; closed port.
   [get-progress-evt #f]

   ;; (amt-k progress-evt? evt? (bytes? -> any) -*> boolean)
   ;; Called in atomic mode.
   ;; Goes with `get-progress-evt`. The final `evt?` argument is
   ;; constrained to a few kinds of events; see docs for
   ;; `port-commit-peeked` for more information. On success, a
   ;; completion function is called in atomic mode, but possibly in a
   ;; different thread, with the committed bytes. The result is a
   ;; boolean indicating success or failure.
   [commit (lambda (amt progress-evt ext-evt finish) #f)])

  (property
   [prop:input-port-evt (lambda (i)
                          ;; not atomic mode
                          (let ([i (->core-input-port i)])
                            (cond
                              [(core-port-closed? i)
                               always-evt]
                              [else
                               (define byte-ready (method core-input-port i byte-ready))
                               (cond
                                 [(input-port? byte-ready)
                                  byte-ready]
                                 [else
                                  (poller-evt
                                   (poller
                                    (lambda (self poll-ctx)
                                      ;; atomic mode
                                      (define v (byte-ready i
                                                            (lambda ()
                                                              (schedule-info-did-work! (poll-ctx-sched-info poll-ctx)))))
                                      (cond
                                        [(evt? v)
                                         (values #f v)]
                                        [(eq? v #t)
                                         (values (list #t) #f)]
                                        [else
                                         (values #f self)]))))])])))]))

;; ----------------------------------------

(define empty-input-port
  (new core-input-port
       [name 'empty]))

;; ----------------------------------------

(class compat-input-port #:extends core-input-port
  (field
   [self #f]))

(define (make-core-input-port #:name name
                              #:data [data #f]
                              #:self self
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
  (new compat-input-port
       #:override
       ([close (and #t (lambda (in) (close self)))]
        [count-lines! (and count-lines! (lambda (in) (count-lines! self)))]
        [get-location (and get-location (lambda (in) (get-location self)))]
        [file-position (and file-position
                            (if (input-port? file-position)
                                file-position
                                (if (procedure-arity-includes? file-position 2)
                                    (case-lambda
                                      [(in) (file-position self)]
                                      [(in pos) (file-position self pos)])
                                    (lambda (out) (file-position self)))))]
        [buffer-mode
         (and buffer-mode (case-lambda
                            [(in) (buffer-mode self)]
                            [(in mode) (buffer-mode self mode)]))]
        [prepare-change (and prepare-change (lambda (in) (prepare-change self)))]
        [read-in
         (if (input-port? read-in)
             read-in
             (lambda (in bstr start end copy?)
               (read-in self bstr start end copy?)))]
        [peek-in
         (if (input-port? peek-in)
             peek-in
             (lambda (in bstr start end skip progress-evt copy?)
               (peek-in self bstr start end skip progress-evt copy?)))]
        [byte-ready
         (if (input-port? byte-ready)
             byte-ready
             (lambda (in work-done!) (byte-ready self work-done!)))]
        [get-progress-evt (and get-progress-evt (lambda (in) (get-progress-evt self)))]
        [commit (and #t (lambda (in amt-k progress-evt? evt? finish)
                          (commit self amt-k progress-evt? evt? finish)))])
       ;; fields
       [name name]
       [offset init-offset]
       [data data]
       [self self]))
