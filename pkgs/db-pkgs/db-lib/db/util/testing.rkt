#lang racket/base
(require racket/contract
         racket/class
         ffi/unsafe/atomic
         db/private/generic/interfaces
         db/private/generic/common)

(define high-latency-connection%
  (class* locking% (connection<%>)
    (init-private connection
                  latency
                  sleep-atomic?)
    (inherit call-with-lock)
    (super-new)

    (define-syntax-rule (define-forward defmethod (sleep-factor method arg ...) ...)
      (begin
        (defmethod (method arg ...)
          (call-with-lock 'method
            (lambda ()
              (let ([c connection])
                (unless c (error/not-connected 'method))
                (when (positive? sleep-factor)
                  (if sleep-atomic?
                      (call-as-atomic (lambda () (sleep (* sleep-factor latency))))
                      (sleep (* sleep-factor latency))))
                (send c method arg ...)))))
        ...))

    (define-forward define/public
      (0 get-dbsystem)
      (2 query fsym stmt cursor?) ;; 2 because may require implicit prepare
      (1 prepare fsym stmt close-on-exec?)
      (1 fetch/cursor fsym stmt fetch-size)
      (0 get-base)
      (0 free-statement stmt need-lock?)
      (0 transaction-status fsym)
      (1 start-transaction fsym isolation cwt?)
      (1 end-transaction fsym mode cwt?)
      (1 list-tables fsym schema))

    (define/override (connected?) (and connection (send connection connected?)))

    (define/public (disconnect)
      (set! connection #f))))

(define (high-latency-connection connection latency
                                 #:sleep-atomic? [sleep-atomic? #f])
  (new high-latency-connection%
       [connection connection]
       [latency latency]
       [sleep-atomic? sleep-atomic?]))

(provide/contract
 [high-latency-connection
  (->* (connection? (>=/c 0))
       (#:sleep-atomic? any/c)
       connection?)])
