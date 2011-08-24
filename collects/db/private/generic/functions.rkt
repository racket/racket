#lang racket/base
(require (for-syntax racket/base)
         racket/contract
         unstable/prop-contract
         racket/class
         "interfaces.rkt")

;; == Administrative procedures

(define (connection? x)
  (is-a? x connection<%>))

(define (connected? x)
  (send x connected?))

(define (disconnect x)
  (send x disconnect))

(define (connection-dbsystem x)
  (send x get-dbsystem))

(define (dbsystem? x)
  (is-a? x dbsystem<%>))

(define (dbsystem-name x)
  (send x get-short-name))

(define (dbsystem-supported-types x)
  (send x get-known-types))

;; == Misc procedures

;; Value of prop:statement should be a function from struct instance to statement.
(define-values (prop:statement prop:statement? prop:statement-ref)
  (make-struct-type-property 'prop:statement))

(define (statement? x)
  (or (string? x)
      (prepared-statement? x)
      (statement-binding? x)
      (prop:statement? x)))

(define complete-statement?
  (or/c string? statement-binding?))

(define (bind-prepared-statement pst params)
  (send pst bind 'bind-prepared-statement params))

(define (prepared-statement? x)
  (is-a? x prepared-statement<%>))

(define (prepared-statement-parameter-types pst)
  (send pst get-param-types))
(define (prepared-statement-result-types pst)
  (send pst get-result-types))

;; A virtual-statement is:
;;   - (virtual-statement table gen)
;;     where table is a weak-hasheq[connection => prepared-statement]
;;     and gen is (dbsystem -> string)
(struct virtual-statement (table gen)
        #:property prop:statement
        (lambda (stmt c)
          (let ([table (virtual-statement-table stmt)]
                [gen (virtual-statement-gen stmt)]
                [cache? (not (is-a? c no-cache-prepare<%>))])
            (let ([table-pst (hash-ref table c #f)])
              (or table-pst
                  (let* ([sql-string (gen (send c get-dbsystem))]
                         [pst (prepare1 'virtual-statement c sql-string (not cache?))])
                    (when cache? (hash-set! table c pst))
                    pst))))))

(define virtual-statement*
  (let ([virtual-statement
         (lambda (gen)
           (virtual-statement (make-weak-hasheq)
                              (if (string? gen) (lambda (_) gen) gen)))])
    virtual-statement))

;; == Query procedures

;; query1 : connection symbol Statement -> QueryResult
(define (query1 c fsym stmt)
  (send c query fsym stmt))

;; query/recordset : connection symbol Statement nat/#f -> void
(define (query/recordset c fsym sql want-columns)
  (let [(result (query1 c fsym sql))]
    (unless (recordset? result)
      (uerror fsym "query did not return recordset: ~e" sql))
    (let ([got-columns (length (recordset-headers result))])
      (when (and want-columns (not (= got-columns want-columns)))
        (uerror fsym "query returned ~a ~a (expected ~a): ~e"
                got-columns (if (= got-columns 1) "column" "columns") want-columns sql)))
    result))

(define (recordset->row fsym rs sql maybe-row? one-column?)
  (define rows (recordset-rows rs))
  (cond [(null? rows)
         (cond [maybe-row? #f]
               [else (uerror fsym "query returned zero rows (expected 1): ~e" sql)])]
        [(null? (cdr rows))
         (let ([row (car rows)])
           (cond [one-column? (vector-ref row 0)]
                 [else row]))]
        [else
         (uerror fsym "query returned multiple rows (expected 1): ~e" sql)]))

(define (compose-statement fsym c stmt args checktype)
  (cond [(prop:statement? stmt)
         (let ([stmt* ((prop:statement-ref stmt) stmt c)])
           (compose-statement fsym c stmt* args checktype))]
        [(or (pair? args)
             (prepared-statement? stmt)
             (virtual-statement? stmt))
         (let ([pst
                (cond [(string? stmt)
                       (prepare1 fsym c stmt #t)]
                      [(prepared-statement? stmt)
                       ;; Ownership check done later, by query method.
                       stmt]
                      [(statement-binding? stmt)
                       (error fsym
                              (string-append
                               "cannot execute statement-binding with "
                               "additional inline arguments: ~e")
                              stmt)])])
           (send pst check-results fsym checktype stmt)
           (send pst bind fsym args))]
        [else ;; no args, and stmt is either string or statement-binding
         stmt]))


;; Query API procedures

;; query-rows : connection Statement arg ... -> (listof (vectorof 'a))
(define (query-rows c sql . args)
  (let ([sql (compose-statement 'query-rows c sql args 'recordset)])
    (recordset-rows (query/recordset c 'query-rows sql #f))))

;; query-list : connection Statement arg ... -> (listof 'a)
;; Expects to get back a recordset with one field per row.
(define (query-list c sql . args)
  (let ([sql (compose-statement 'query-list c sql args 1)])
    (map (lambda (v) (vector-ref v 0))
         (recordset-rows (query/recordset c 'query-list sql 1)))))

;; query-row : connection Statement arg ... -> (vector-of 'a)
;; Expects to get back a recordset of zero or one rows.
(define (query-row c sql . args)
  (let ([sql (compose-statement 'query-row c sql args 'recordset)])
    (recordset->row 'query-row
                    (query/recordset c 'query-row sql #f)
                    sql #f #f)))

;; query-maybe-row : connection Statement arg ... -> (vector-of 'a) or #f
;; Expects to get back a recordset of zero or one rows.
(define (query-maybe-row c sql . args)
  (let ([sql (compose-statement 'query-maybe-row c sql args 'recordset)])
    (recordset->row 'query-maybe-row
                    (query/recordset c 'query-maybe-row sql #f)
                    sql #t #f)))

;; query-value : connection string arg ... -> value | raises error
;; Expects to get back a recordset of exactly one row, exactly one column.
(define (query-value c sql . args)
  (let ([sql (compose-statement 'query-value c sql args 1)])
    (recordset->row 'query-value
                    (query/recordset c 'query-value sql 1)
                    sql #f #t)))

;; query-maybe-value : connection Statement arg ... -> value/#f
;; Expects to get back a recordset of zero or one rows, exactly one column.
(define (query-maybe-value c sql . args)
  (let ([sql (compose-statement 'query-maybe-value c sql args 1)])
    (recordset->row 'query-maybe-value
                    (query/recordset c 'query-maybe-value sql 1)
                    sql #t #t)))

;; query-exec : connection Statement arg ... -> void
(define (query-exec c sql . args)
  (let ([sql (compose-statement 'query-exec c sql args #f)])
    (query1 c 'query-exec sql)
    (void)))

;; query : connection Statement arg ... -> QueryResult
(define (query c sql . args)
  (let ([sql (compose-statement 'query c sql args #f)])
    (query1 c 'query sql)))

;; ========================================

(define (in-query c stmt . args)
  (let ([rows (in-query-helper #f c stmt args)])
    (make-do-sequence
     (lambda ()
       (values (lambda (p) (vector->values (car p)))
               cdr
               rows
               pair?
               (lambda _ #t)
               (lambda _ #t))))))

(define-sequence-syntax in-query*
  (lambda () #'in-query)
  (lambda (stx)
    (syntax-case stx ()
      [[(var ...) (in-query c stmt arg ...)]
       #'[(var ...)
          (:do-in ([(rows) (in-query-helper (length '(var ...)) c stmt (list arg ...))])
                  (void) ;; outer check
                  ([rows rows]) ;; loop inits
                  (pair? rows) ;; pos guard
                  ([(var ...) (vector->values (car rows))]) ;; inner bindings
                  #t ;; pre guard
                  #t ;; post guard
                  ((cdr rows)))]] ;; loop args
      [_ #f])))

(define (in-query-helper vars c stmt args)
  ;; Not protected by contract
  (unless (connection? c)
    (apply raise-type-error 'in-query "connection" 0 c stmt args))
  (unless (statement? stmt)
    (apply raise-type-error 'in-query "statement" 1 c stmt args))
  (let* ([check (or vars 'recordset)]
         [stmt (compose-statement 'in-query c stmt args check)])
    (recordset-rows (query/recordset c 'in-query stmt vars))))

;; ========================================

(define (prepare c stmt)
  ;; FIXME: handle non-string statements
  (prepare1 'prepare c stmt #f))

;; ----

(define (prepare1 fsym c stmt close-on-exec?)
  ;; stmt is string
  (send c prepare fsym stmt close-on-exec?))

;; ========================================

(define (start-transaction c #:isolation [isolation #f])
  (send c start-transaction 'start-transaction isolation))

(define (commit-transaction c)
  (send c end-transaction 'commit-transaction 'commit))

(define (rollback-transaction c)
  (send c end-transaction 'rollback-transaction 'rollback))

(define (in-transaction? c)
  (and (send c transaction-status 'in-transaction?) #t))

(define (needs-rollback? c)
  (eq? (send c transaction-status 'needs-rollback?) 'invalid))

(define (call-with-transaction c proc #:isolation [isolation #f])
  (send c start-transaction 'call-with-transaction isolation)
  (begin0 (with-handlers ([(lambda (e) #t)
                           (lambda (e)
                             (send c end-transaction 'call-with-transaction 'rollback)
                             (raise e))])
            (proc))
    (send c end-transaction 'call-with-transaction 'commit)))

;; ========================================

(define (get-schemas c)
  (recordset-rows
   (send c query 'get-schemas
         "select catalog_name, schema_name from information_schema.schemata")))

(define (get-tables c)
  (recordset-rows
   (send c query 'get-tables
         "select table_catalog, table_schema, table_name from information_schema.tables")))

;; ========================================

(define preparable/c (or/c string? virtual-statement?))

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
  (->* (connection? statement?) () #:rest list? (listof vector?))]
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

 #|
 [in-query
  (->* (connection? statement?) () #:rest list? sequence?)]
 |#

 [prepare
  (-> connection? preparable/c any)]
 [bind-prepared-statement
  (-> prepared-statement? list? any)]

 [rename virtual-statement* virtual-statement
  (-> (or/c string? (-> dbsystem? string?))
      virtual-statement?)]
 [virtual-statement?
  (-> any/c boolean?)]

 [start-transaction
  (->* (connection?)
       (#:isolation (or/c 'serializable 'repeatable-read 'read-committed 'read-uncommitted #f))
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
       (#:isolation (or/c 'serializable 'repeatable-read 'read-committed 'read-uncommitted #f))
       void?)]

 [prop:statement
  (struct-type-property/c
   (-> any/c connection?
       statement?))]

#|
 [get-schemas
  (-> connection? (listof vector?))]
 [get-tables
  (-> connection? (listof vector?))]
|#)
