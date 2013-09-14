#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/parse/experimental/template)
         racket/dict
         syntax/location
         racket/contract/base)

;; ============================================================

(require db/private/generic/interfaces
         db/private/generic/sql-data)

(provide (struct-out simple-result)
         (struct-out rows-result)
         statement-binding?
         (struct-out exn:fail:sql))

(provide sql-null
         sql-null?
         sql-null->false
         false->sql-null)

(provide/contract
 [struct sql-date ([year exact-integer?]
                   [month (integer-in 0 12)]
                   [day (integer-in 0 31)])]
 [struct sql-time ([hour (integer-in 0 23)]
                   [minute (integer-in 0 59)]
                   [second (integer-in 0 61)] ;; leap seconds
                   [nanosecond (integer-in 0 (sub1 #e1e9))]
                   [tz (or/c #f exact-integer?)])]
 [struct sql-timestamp ([year exact-integer?]
                        [month (integer-in 0 12)]
                        [day (integer-in 0 31)]
                        [hour (integer-in 0 23)]
                        [minute (integer-in 0 59)]
                        [second (integer-in 0 61)]
                        [nanosecond (integer-in 0 (sub1 #e1e9))]
                        [tz (or/c #f exact-integer?)])]
 [struct sql-interval ([years exact-integer?]
                       [months exact-integer?]
                       [days exact-integer?]
                       [hours exact-integer?]
                       [minutes exact-integer?]
                       [seconds exact-integer?]
                       [nanoseconds exact-integer?])]

 [sql-day-time-interval?
  (-> any/c boolean?)]
 [sql-year-month-interval?
  (-> any/c boolean?)]
 [sql-interval->sql-time
  (->* (sql-interval?) (any/c)
       any)]
 [sql-time->sql-interval
  (-> sql-time? sql-day-time-interval?)]

 [make-sql-bits
  (-> exact-nonnegative-integer? sql-bits?)]
 [sql-bits?
  (-> any/c boolean?)]
 [sql-bits-length
  (-> sql-bits? exact-nonnegative-integer?)]
 [sql-bits-ref
  (-> sql-bits? exact-nonnegative-integer? boolean?)]
 [sql-bits-set!
  (-> sql-bits? exact-nonnegative-integer? boolean? void?)]
 [sql-bits->list
  (-> sql-bits? (listof boolean?))]
 [list->sql-bits
  (-> (listof boolean?) sql-bits?)]
 [sql-bits->string
  (-> sql-bits? string?)]
 [string->sql-bits
  (-> string? sql-bits?)])

;; ============================================================

(require db/private/generic/functions)

(define fetch-size/c
  (or/c exact-positive-integer? +inf.0))

(define grouping-field/c (or/c string? exact-nonnegative-integer?))
(define group/c (or/c grouping-field/c (vectorof grouping-field/c)))
(define grouping/c (or/c group/c (listof group/c)))

(define group-mode/c
  (listof (or/c 'list 'preserve-null)))

(define in-query/c
  (->* (connection? statement?)
       (#:fetch fetch-size/c
        #:group grouping/c
        #:group-mode group-mode/c)
       #:rest list?
       sequence?))

(define here-mod-path (quote-module-path))

(define-syntax contracted-in-query
  (make-provide/contract-transformer
   (quote-syntax in-query/c)
   (quote-syntax in-query)
   (quote-syntax in-query)
   (quote-syntax here-mod-path)))

(define-sequence-syntax in-query*
  (lambda () #'contracted-in-query)
  (lambda (stx)
    (syntax-parse stx
      [[(var ...) (~and form
                        (in-query (~or (~optional (~seq #:fetch fetch-size))
                                       (~optional (~seq #:group grouping-fields))
                                       (~optional (~seq #:group-mode group-mode))
                                       (~between arg:expr 2 +inf.0))
                                  ...))]
       #:declare fetch-size (expr/c #'fetch-size/c #:context #'form) #:role "fetch size argument"
       #:declare grouping-fields (expr/c #'grouping/c #:context #'form) #:role "grouping fields argument"
       #:declare group-mode (expr/c #'group-mode/c #:context #'form) #:role "group mode argument"
       #:with (c stmt q-arg ...) #'(arg ...)
       #:declare c (expr/c #'connection? #:context #'form) #:role "connection argument"
       #:declare stmt (expr/c #'statement? #:context #'form) #:role "statement argument"
       (template
        [(var ...) (in-query-helper (length '(var ...)) c.c stmt.c q-arg ...
                                    (?? (?@ #:fetch fetch-size.c))
                                    (?? (?@ #:group grouping-fields.c))
                                    (?? (?@ #:group-mode group-mode.c)))])]
      [_ #f])))

(provide (rename-out [in-query* in-query]))

(provide/contract
 [connection?
  (-> any/c any)]
 [disconnect
  (-> connection? any)]
 [connected?
  (-> connection? any)]
 [connection-dbsystem
  (-> connection? dbsystem?)]
 [dbsystem?
  (-> any/c any)]
 [dbsystem-name
  (-> dbsystem? symbol?)]
 [dbsystem-supported-types
  (-> dbsystem? (listof symbol?))]

 [statement?
  (-> any/c any)]
 [prepared-statement?
  (-> any/c any)]
 [prepared-statement-parameter-types
  (-> prepared-statement? (or/c list? #f))]
 [prepared-statement-result-types
  (-> prepared-statement? (or/c list? #f))]

 [query-exec
  (->* (connection? statement?) () #:rest list? any)]
 [query-rows
  (->* (connection? statement?)
       (#:group grouping/c
        #:group-mode group-mode/c)
       #:rest list? (listof vector?))]
 [query-list
  (->* (connection? statement?) () #:rest list? list?)]
 [query-row
  (->* (connection? statement?) () #:rest list? vector?)]
 [query-maybe-row
  (->* (connection? statement?) () #:rest list? (or/c #f vector?))]
 [query-value
  (->* (connection? statement?) () #:rest list? any)]
 [query-maybe-value
  (->* (connection? statement?) () #:rest list? any)]
 [query
  (->* (connection? statement?) () #:rest list? any)]

 [prepare
  (-> connection? (or/c string? virtual-statement?) any)]
 [bind-prepared-statement
  (-> prepared-statement? list? any)]

 [virtual-statement
  (-> (or/c string? (-> dbsystem? string?))
      virtual-statement?)]
 [virtual-statement?
  (-> any/c boolean?)]

 [start-transaction
  (->* (connection?)
       (#:isolation (or/c 'serializable 'repeatable-read 'read-committed 'read-uncommitted #f)
        #:option any/c)
       void?)]
 [commit-transaction
  (-> connection? void?)]
 [rollback-transaction
  (-> connection? void?)]
 [in-transaction?
  (-> connection? boolean?)]
 [needs-rollback?
  (-> connection? boolean?)]
 [call-with-transaction
  (->* (connection? (-> any))
       (#:isolation (or/c 'serializable 'repeatable-read 'read-committed 'read-uncommitted #f)
        #:option any/c)
       any)]

 [prop:statement
  (struct-type-property/c
   (-> any/c connection?
       statement?))]

 [list-tables
  (->* (connection?)
       (#:schema (or/c 'search-or-current 'search 'current))
       (listof string?))]
 [table-exists?
  (->* (connection? string?)
       (#:schema (or/c 'search-or-current 'search 'current)
        #:case-sensitive? any/c)
       boolean?)]

 [group-rows
  (->* (rows-result?
        #:group grouping/c)
       (#:group-mode (listof (or/c 'list 'preserve-null #|deprecated:|# 'preserve-null-rows)))
       rows-result?)]

 [rows->dict
  (->* (rows-result? #:key grouping/c #:value grouping/c)
       (#:value-mode group-mode/c)
       dict?)]
 )

;; ============================================================

(require "private/generic/connect-util.rkt")

(provide/contract
 [kill-safe-connection
  (-> connection? connection?)]
 [virtual-connection
  (->* ((or/c (-> connection?) connection-pool?))
       ()
       connection?)]
 [connection-pool
  (->* ((-> connection?))
       (#:max-connections (or/c (integer-in 1 10000) +inf.0)
        #:max-idle-connections (or/c (integer-in 1 10000) +inf.0))
       connection-pool?)]
 [connection-pool?
  (-> any/c boolean?)]
 [connection-pool-lease
  (->* (connection-pool?)
       ((or/c custodian? evt?))
       connection?)])

;; ============================================================

(require "private/generic/dsn.rkt")

(provide dsn-connect)   ;; can't express "or any kw at all" w/ ->* contract
(provide/contract
 [struct data-source
         ([connector connector?]
          [args arglist?]
          [extensions (listof (list/c symbol? writable-datum?))])]
 [current-dsn-file (parameter/c path-string?)]
 [get-dsn
  (->* (symbol?) (any/c #:dsn-file path-string?) any)]
 [put-dsn
  (->* (symbol? (or/c data-source? #f)) (#:dsn-file path-string?) void?)]
 [postgresql-data-source
  (->* ()
       (#:user string?
        #:database string?
        #:server string?
        #:port exact-positive-integer?
        #:socket (or/c string? 'guess)
        #:password (or/c string? #f)
        #:allow-cleartext-password? boolean?
        #:ssl (or/c 'yes 'optional 'no)
        #:notice-handler (or/c 'output 'error)
        #:notification-handler (or/c 'output 'error)
        #:debug? any/c)
       data-source?)]
 [mysql-data-source
  (->* ()
       (#:user string?
        #:database (or/c string? #f)
        #:server string?
        #:port exact-positive-integer?
        #:socket (or/c string? 'guess)
        #:password (or/c string? #f)
        #:notice-handler (or/c 'output 'error)
        #:debug? any/c)
       data-source?)]
 [sqlite3-data-source
  (->* ()
       (#:database (or/c string? 'memory 'temporary)
        #:mode (or/c 'read-only 'read/write 'create)
        #:busy-retry-limit (or/c exact-nonnegative-integer? +inf.0)
        #:busy-retry-delay (and/c rational? (not/c negative?))
        #:use-place boolean?)
       data-source?)]
 [odbc-data-source
  (->* ()
       (#:dsn string?
        #:user string?
        #:password string?
        #:notice-handler (or/c 'output 'error)
        #:strict-parameter-types? boolean?
        #:character-mode (or/c 'wchar 'utf-8 'latin-1)
        #:use-place boolean?)
       data-source?)])
