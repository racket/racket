#lang racket/base
(require racket/class
         racket/match
         racket/place
         racket/promise
         racket/vector
         ffi/unsafe/atomic
         "interfaces.rkt"
         "prepared.rkt"
         "sql-data.rkt")
(provide place-connect
         place-proxy-connection%

         sql-datum->sexpr
         sexpr->sql-datum)

(define connection-server-channel
  (delay/sync
   (dynamic-place 'db/private/generic/place-server 'connection-server)))

(define (place-connect connection-spec proxy%)
  (let-values ([(channel other-channel) (place-channel)])
    (place-channel-put (force connection-server-channel)
                       (list 'connect other-channel connection-spec))
    (match (place-channel-get channel)
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
             (place-channel-put channel (cons method-name args))
             (match (place-channel-get channel)
               [(cons 'values vals)
                (apply values (for/list ([val (in-list vals)]) (translate-result val)))]
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
              [(statement-binding pst _meta params)
               (list 'statement-binding
                     (send pst get-handle)
                     (map sql-datum->sexpr params))])))

    (define/public (prepare fsym stmt close-on-exec?)
      (call 'prepare fsym stmt close-on-exec?))

    (define/public (free-statement pst)
      (start-atomic)
      (let ([handle (send pst get-handle)])
        (send pst set-handle #f)
        (end-atomic)
        (when channel
          (call/d 'free-statement handle))))

    (define/public (transaction-status fsym)
      (call 'transaction-status fsym))

    (define/public (start-transaction fsym iso)
      (call 'start-transaction fsym iso))

    (define/public (end-transaction fsym mode)
      (call 'end-transaction fsym mode))

    (define/public (list-tables fsym schema)
      (call 'list-tables fsym schema))

    (define/private (translate-result x)
      (match x
        [(list 'simple-result y)
         (simple-result y)]
        [(list 'rows-result h rows)
         (let ([rows
                (for/list ([row (in-list rows)])
                  (vector-map sexpr->sql-datum row))])
           (rows-result h rows))]
        [(list 'prepared-statement handle close-on-exec? param-typeids result-dvecs)
         (new prepared-statement%
              (handle handle)
              (close-on-exec? close-on-exec?)
              (param-typeids param-typeids)
              (result-dvecs result-dvecs)
              (owner this))]
        [_ x]))
    ))

(define (sql-datum->sexpr x)
  (match x
    [(? sql-null?)
     'sql-null]
    [(sql-date Y M D)
     (list 'sql-date Y M D)]
    [(sql-time h m s ns tz)
     (list 'sql-time h m s ns tz)]
    [(sql-timestamp Y M D h m s ns tz)
     (list 'sql-timestamp Y M D h m s ns tz)]
    ;; FIXME: add sql-interval when implemented for odbc
    [_ x]))

(define (sexpr->sql-datum x)
  (match x
    ['sql-null sql-null]
    [(list 'sql-date Y M D) (sql-date Y M D)]
    [(list 'sql-time h m s ns tz) (sql-time h m s ns tz)]
    [(list 'sql-timestamp Y M D h m s ns tz)
     (sql-timestamp Y M D h m s ns tz)]
    [else x]))
