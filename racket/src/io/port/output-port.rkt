#lang racket/base
(require "../common/check.rkt"
         "../common/class.rkt"
         "../host/thread.rkt"
         "../host/pthread.rkt"
         "port.rkt"
         "evt.rkt")

(provide prop:output-port
         output-port?
         ->core-output-port
         (struct-out core-output-port)
         get-write-evt-via-write-out)

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
;; since it can invoke an arbitrary function
(define (->core-output-port v [who #f]
                            #:default [default empty-output-port])
  (cond
    [(core-output-port? v) v]
    [(output-port-ref v #f)
     => (lambda (p)
          (cond
            [(struct-accessor-procedure? p)
             (->core-output-port (p v))]
            [else
             (->core-output-port p)]))]
    [who (raise-argument-error who "output-port?" v)]
    [else default]))

(class core-output-port #:extends core-port
  #:field
  [evt always-evt] ; An evt that is ready when writing a byte won't block
  [write-handler #f]
  [print-handler #f]
  [display-handler #f]

  #:public
  ;; port or (bstr start-k end-k no-block/buffer? enable-break? copy? -*> ...)
  ;; Called in atomic mode.
  ;; Doesn't block if `no-block/buffer?` is true. Does enable breaks
  ;; while blocking if `enable-break?` is true. The `copy?` flag
  ;; indicates that the given byte string should not be exposed to
  ;; untrusted code, and instead of should be copied if necessary.
  ;; The return values are the same as documented for
  ;; `make-output-port`.
  [write-out (lambda (bstr start-k end-k no-block/buffer? enable-break? copy?)
               (- end-k start-k))]

  ;; #f or (any no-block/buffer? enable-break? -*> boolean?)
  ;; Called in atomic mode.
  [write-out-special #f]
  
  ;; #f or (bstr start-k end-k -*> evt?)
  ;; Called in atomic mode.
  ;; The given bstr should not be exposed to untrusted code.
  [get-write-evt (lambda (bstr start-k end-k) always-evt)]

  ;; #f or (any -*> evt?)
  ;; *Not* called in atomic mode.
  [get-write-special-evt #f]

  #:property
  [prop:output-port-evt (lambda (o)
                          ;; not atomic mode
                          (let ([o (->core-output-port o)])
                            (choice-evt
                             (list
                              (poller-evt
                               (poller
                                (lambda (self sched-info)
                                  ;; atomic mode
                                  (cond
                                    [(core-port-closed? o)
                                     (values '(#t) #f)]
                                    [else (values #f self)]))))
                              (core-output-port-evt o)))))])

;; If `write-out` is always atomic (in no-block, no-buffer mode),
;; then an event can poll `write-out`
(define (get-write-evt-via-write-out count-write-evt-via-write-out)
  (lambda (out src-bstr src-start src-end)
    (write-evt
     ;; in atomic mode:
     (lambda (self-evt)
       (define v (send core-output-port out write-out src-bstr src-start src-end #f #f #t))
       (when (exact-integer? v)
         (count-write-evt-via-write-out out v src-bstr src-start))
       (if (evt? v)
           (values #f (replace-evt v self-evt))
           (values (list v) #f))))))

(struct write-evt (proc)
  #:property prop:evt (poller
                       (lambda (self sched-info)
                         ((write-evt-proc self) self))))

(define empty-output-port
  (new core-output-port
       #:field
       [name 'empty]))
