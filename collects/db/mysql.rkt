#lang racket/base
(require racket/contract
         "base.rkt"
         "private/mysql/main.rkt")

;; FIXME: Contracts duplicated at main.rkt
(provide/contract
 [mysql-connect
  (->* (#:user string?
        #:database string?)
       (#:password (or/c string? (list/c 'hash string?) #f)
        #:server (or/c string? #f)
        #:port (or/c exact-positive-integer? #f)
        #:socket (or/c path-string? 'guess #f)
        #:notice-handler (or/c 'output 'error output-port? procedure?))
       connection?)]
 [mysql-guess-socket-path
  (-> path-string?)]
 [mysql-password-hash
  (-> string? string?)])
