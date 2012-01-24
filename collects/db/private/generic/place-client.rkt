#lang racket/base
(require racket/class
         syntax/parse/private/minimatch
         racket/place
         racket/promise
         racket/serialize
         racket/runtime-path
         (for-syntax (only-in racket/base quote))
         ffi/unsafe/atomic
         "interfaces.rkt"
         "prepared.rkt")
(provide place-connect
         place-proxy-connection%)

(define (pchan-put chan datum)
  (place-channel-put chan (serialize datum)))
(define (pchan-get chan)
  (deserialize (place-channel-get chan)))

(define-runtime-module-path-index _place-server
  'db/private/generic/place-server)

(define connection-server-channel
  (delay/sync
   (dynamic-place 'db/private/generic/place-server 'connection-server)))

(define (place-connect connection-spec proxy%)
  (let-values ([(channel other-channel) (place-channel)])
    (place-channel-put (force connection-server-channel)
                       (list 'connect other-channel connection-spec))
    (match (pchan-get channel)
      [(list 'ok)
       (new proxy% (channel channel))]
      [(list 'error message)
       (raise (make-exn:fail message (current-continuation-marks)))])))

(define place-proxy-connection%
  (class* locking% (connection<%>)
    (init-field channel)
    (inherit call-with-lock
             call-with-lock*)
    (super-new)

    (define/private (call method-name . args)
      (call-with-lock method-name (lambda () (call* method-name args #t))))
    (define/private (call/d method-name . args)
      (call-with-lock* method-name (lambda () (call* method-name args #f)) #f #f))
    (define/private (call* method-name args need-connected?)
      (cond [channel
             (pchan-put channel (cons method-name args))
             (match (pchan-get channel)
               [(cons 'values vals)
                (apply values (for/list ([val (in-list vals)]) (sexpr->result val)))]
               [(list 'error message)
                (raise (make-exn:fail message (current-continuation-marks)))])]
            [need-connected?
             (unless channel
               (error/not-connected method-name))]
            [else (void)]))

    (define/override (connected?)
      ;; FIXME: can underlying connection disconnect w/o us knowing?
      (and channel #t))

    (define/public (disconnect)
      (call/d 'disconnect)
      (set! channel #f))

    (define/public (get-dbsystem) (error 'get-dbsystem "not implemented"))
    (define/public (get-base) this)

    (define/public (query fsym stmt)
      (call 'query fsym
            (match stmt
              [(? string?) (list 'string stmt)]
              [(statement-binding pst meta params)
               (list 'statement-binding (send pst get-handle) meta params)])))
    (define/public (prepare fsym stmt close-on-exec?)
      (call 'prepare fsym stmt close-on-exec?))
    (define/public (transaction-status fsym)
      (call 'transaction-status fsym))
    (define/public (start-transaction fsym iso cwt?)
      (call 'start-transaction fsym iso cwt?))
    (define/public (end-transaction fsym mode cwt?)
      (call 'end-transaction fsym mode cwt?))
    (define/public (list-tables fsym schema)
      (call 'list-tables fsym schema))

    (define/public (free-statement pst)
      (start-atomic)
      (let ([handle (send pst get-handle)])
        (send pst set-handle #f)
        (end-atomic)
        (when channel
          (call/d 'free-statement handle))))

    (define/private (sexpr->result x)
      (match x
        [(list 'simple-result y)
         (simple-result y)]
        [(list 'rows-result h rows)
         (rows-result h rows)]
        [(list 'prepared-statement handle close-on-exec? param-typeids result-dvecs)
         (new prepared-statement%
              (handle handle)
              (close-on-exec? close-on-exec?)
              (param-typeids param-typeids)
              (result-dvecs result-dvecs)
              (owner this))]
        [_ x]))))
