#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "port.rkt"
         "evt.rkt")

(provide prop:output-port
         output-port?
         ->core-output-port
         (struct-out core-output-port)
         make-core-output-port)

(define-values (prop:output-port output-port-via-property? output-port-ref)
  (make-struct-type-property 'output-port
                             (lambda (v sti)
                               (check 'prop:output-port (lambda (v) (or (exact-nonnegative-integer? v)
                                                                        (output-port? v)))
                                      #:contract "(or/c output-port? exact-nonnegative-integer?)"
                                      v)
                               (check-immutable-field 'prop:output-port v sti)
                               (if (exact-nonnegative-integer? v)
                                   (make-struct-field-accessor (list-ref sti 3) v)
                                   v))
                             (list (cons prop:secondary-evt
                                         (lambda (v) port->evt))
                                   (cons prop:output-port-evt
                                         (lambda (o)
                                           (output-port-evt-ref (->core-output-port o)))))))

(define (output-port? p)
  (or (core-output-port? p)
      (output-port-via-property? p)))

;; This function should not be called in atomic mode,
;; since it can invoke an artitrary function
(define (->core-output-port v)
  (cond
    [(core-output-port? v) v]
    [(output-port? v)
     (let ([p (output-port-ref v)])
       (cond
         [(struct-accessor-procedure? p)
          (->core-output-port (p v))]
         [else
          (->core-output-port p)]))]
    [else
     empty-output-port]))

(struct core-output-port core-port
  (
   ;; Various functions below are called in atomic mode; see
   ;; `core-input-port` for more information on atomicity.

   evt ; An evt that is ready when writing a byte won't block
   
   write-out ; (bstr start-k end-k no-block/buffer? enable-break? copy? -> ...)
   ;;          Called in atomic mode.
   ;;          Doesn't block if `no-block/buffer?` is true.
   ;;          Does enable breaks while blocking if `enable-break?` is true.
   ;;          The `copy?` flag indicates that the given byte string should
   ;;          not be exposed to untrusted code, and instead of should be
   ;;          copied if necessary. The return values are the same as
   ;;          documented for `make-output-port`.

   write-out-special ; (any no-block/buffer? enable-break? -> boolean?)
   ;;          Called in atomic mode.

   get-write-evt ; (bstr start-k end-k -> evt?)
   ;;            Called in atomic mode.
   ;;            The given bstr should not be exposed to untrusted code.

   get-write-special-evt ; (-> evt?)
   ;;            *Not* called in atomic mode.
   
   [write-handler #:mutable]
   [print-handler #:mutable]
   [display-handler #:mutable])
  #:authentic
  #:property prop:output-port-evt (lambda (o)
                                    (choice-evt
                                     (list
                                      (poller-evt
                                       (poller
                                        (lambda (self sched-info)
                                          (cond
                                            [(closed-state-closed? (core-port-closed o))
                                             (values '(#t) #f)]
                                            [else (values #f self)]))))
                                      (core-output-port-evt o)))))

(struct write-evt (proc)
  #:property prop:evt (poller
                       (lambda (self sched-info)
                         ((write-evt-proc self) self))))

(define (make-core-output-port #:name name
                               #:data [data #f]
                               #:evt evt
                               #:write-out write-out
                               #:close close
                               #:write-out-special [write-out-special #f]
                               #:get-write-evt [get-write-evt #f]
                               #:count-write-evt-via-write-out [count-write-evt-via-write-out #f]
                               #:get-write-special-evt [get-write-special-evt #f]
                               #:get-location [get-location #f]
                               #:count-lines! [count-lines! #f]
                               #:file-position [file-position #f]
                               #:init-offset [init-offset 0]
                               #:buffer-mode [buffer-mode #f])
  (core-output-port name
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
                    
                    evt
                    write-out
                    write-out-special
                    (or get-write-evt
                        (and count-write-evt-via-write-out
                             ;; If `write-out` is always atomic (in no-block, no-buffer mode),
                             ;; then an event can poll `write-out`:
                             (lambda (src-bstr src-start src-end)
                               (write-evt
                                ;; in atomic mode:
                                (lambda (self)
                                  (define v (write-out src-bstr src-start src-end #f #f #t))
                                  (when (exact-integer? v)
                                    (count-write-evt-via-write-out v src-bstr src-start))
                                  (if (evt? v)
                                      ;; FIXME: should be `(replace-evt v self)`
                                      (values #f self)
                                      (values (list v) #f)))))))
                    get-write-special-evt
                    
                    #f   ; write-handler
                    #f   ; display-handler
                    #f)) ; print-handler
  
(define empty-output-port
  (make-core-output-port #:name 'empty
                         #:evt always-evt
                         #:write-out (lambda (bstr start end no-buffer? enable-break?)
                                       (- end start))
                         #:write-out-special (lambda (v no-buffer? enable-break?)
                                               #t)
                         #:close void))
