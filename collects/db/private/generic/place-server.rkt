#lang racket/base
(require (for-syntax racket/base)
         racket/class
         syntax/parse/private/minimatch
         racket/place
         racket/serialize
         unstable/lazy-require
         "interfaces.rkt"
         "prepared.rkt")
(provide connection-server)

(define (pchan-put chan datum)
  (place-channel-put chan (serialize datum)))
(define (pchan-get chan)
  (deserialize (place-channel-get chan)))

#|
Connection creation protocol

client -> server on client-chan: (list 'connect conn-chan <connect-options>)
server -> client on conn-chan:   (or (list 'ok)
                                     (list 'error string))

where <connect-spec> ::= (list 'sqlite3 path/sym mode-sym delay-num limit-num)
                      |  (list 'odbc string string/#f string/#f boolean symbol)
|#
(define (connection-server client-chan)
  (let loop ()
    (serve client-chan)
    (loop)))

(lazy-require
 ["../sqlite3/main.rkt" (sqlite3-connect)]
 ["../odbc/main.rkt" (odbc-connect
                      odbc-driver-connect)])

(define (serve client-chan)
  (match (place-channel-get client-chan)
    [(list 'connect conn-chan connect-spec)
     (with-handlers ([exn:fail?
                      (lambda (e)
                        (pchan-put conn-chan (list 'error (exn-message e))))])
       (let* ([c
               (match connect-spec
                 [(list 'sqlite3 db mode busy-retry-delay busy-retry-limit)
                  (sqlite3-connect #:database db
                                   #:mode mode
                                   #:busy-retry-delay busy-retry-delay
                                   #:busy-retry-limit busy-retry-limit
                                   #:use-place #f)]
                 [(list 'odbc dsn user password strict-param? char-mode)
                  (odbc-connect #:dsn dsn
                                #:user user
                                #:password password
                                #:strict-parameter-types? strict-param?
                                #:character-mode char-mode
                                #:use-place #f)]
                 [(list 'odbc-driver connection-string strict-param? char-mode)
                  (odbc-driver-connect connection-string
                                       #:strict-parameter-types? strict-param?
                                       #:character-mode char-mode
                                       #:use-place #f)])]
              [p (new proxy-server% (connection c) (channel conn-chan))])
         (pchan-put conn-chan (list 'ok))
         (thread (lambda () (send p serve)))))]))

#|
Connection methods protocol

client -> server: (list '<method-name> arg ...)
server -> client: (or (list 'values result ...)
                      (list 'error string))
|#

(define proxy-server%
  (class object%
    (init-field connection
                channel)
    (super-new)

    (define pstmt-table (make-hash)) ;; int => prepared-statement
    (define pstmt-counter 0)

    (define/public (serve)
      (serve1)
      (when connection (serve)))

    (define/private (serve1)
      (with-handlers ([exn?
                       (lambda (e)
                         (pchan-put channel (list 'error (exn-message e))))])
        (call-with-values
            (lambda ()
              (match (pchan-get channel)
                [(list 'disconnect)
                 (send connection disconnect)
                 (set! connection #f)]
                [(list 'free-statement pstmt-index)
                 (send connection free-statement (hash-ref pstmt-table pstmt-index))
                 (hash-remove! pstmt-table pstmt-index)]
                [(list 'query fsym stmt)
                 (send connection query fsym (sexpr->statement stmt))]
                [msg
                 (define-syntax-rule (forward-methods (method arg ...) ...)
                   (match msg
                     [(list 'method arg ...)
                      (send connection method arg ...)]
                     ...))
                 (forward-methods (connected?)
                                  (prepare w s m)
                                  (list-tables w s)
                                  (start-transaction w m c)
                                  (end-transaction w m c)
                                  (transaction-status w))]))
          (lambda results
            (let ([results (for/list ([result (in-list results)]) (result->sexpr result))])
              (pchan-put channel (cons 'values results)))))))

    (define/private (sexpr->statement x)
      (match x
        [(list 'string s) s]
        [(list 'statement-binding pstmt-index meta args)
         (statement-binding (hash-ref pstmt-table pstmt-index) meta args)]))

    (define/private (result->sexpr x)
      (match x
        [(simple-result y)
         (list 'simple-result y)]
        [(rows-result h rows)
         (list 'rows-result h rows)]
        ;; FIXME: Assumes prepared-statement is concrete class, not interface.
        [(? (lambda (x) (is-a? x prepared-statement%)))
         (let ([pstmt-index (begin (set! pstmt-counter (add1 pstmt-counter)) pstmt-counter)])
           (hash-set! pstmt-table pstmt-index x)
           (list 'prepared-statement
                 pstmt-index
                 (get-field close-on-exec? x)
                 (get-field param-typeids x)
                 (get-field result-dvecs x)))]
        [_ x]))))
