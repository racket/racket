#lang racket/base
(require racket/contract/base
         "base.rkt"
         "private/sqlite3/main.rkt")

;; FIXME: Contracts duplicated at main.rkt
(provide/contract
 [sqlite3-connect
  (->* (#:database (or/c path-string? 'memory 'temporary))
       (#:mode (or/c 'read-only 'read/write 'create)
        #:busy-retry-limit (or/c exact-nonnegative-integer? +inf.0)
        #:busy-retry-delay (and/c rational? (not/c negative?))
        #:use-place any/c
        #:debug? any/c)
       connection?)]
 [sqlite3-available?
  (-> boolean?)])
