#lang racket/base
(require racket/contract/base
         openssl
         db/base
         "private/postgresql/main.rkt")

;; FIXME: Contracts duplicated at main.rkt
(provide/contract
 [postgresql-connect
  (->* (#:user string?
        #:database string?)
       (#:password (or/c string? (list/c 'hash string?) #f)
        #:server (or/c string? #f)
        #:port (or/c exact-positive-integer? #f)
        #:socket (or/c path-string? 'guess #f)
        #:allow-cleartext-password? boolean?
        #:ssl (or/c 'yes 'no 'optional)
        #:ssl-context ssl-client-context?
        #:notice-handler (or/c 'output 'error output-port? procedure?)
        #:notification-handler (or/c 'output 'error output-port? procedure?)
        #:debug? any/c)
       connection?)]
 [postgresql-guess-socket-path
  (-> path-string?)]
 [postgresql-password-hash
  (-> string? string? string?)])
