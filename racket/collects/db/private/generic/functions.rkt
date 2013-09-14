#lang racket/base
(require racket/vector
         racket/class
         racket/promise
         unstable/error
         "interfaces.rkt"
         (only-in "sql-data.rkt" sql-null sql-null?))
(provide connected?
         disconnect
         connection-dbsystem
         dbsystem-name
         dbsystem-supported-types
         prop:statement
         statement?
         bind-prepared-statement
         prepared-statement-parameter-types
         prepared-statement-result-types
         virtual-statement?
         (rename-out [virtual-statement* virtual-statement])
         query-rows
         query-list
         query-row
         query-maybe-row
         query-value
         query-maybe-value
         query-exec
         query
         in-query
         in-query-helper ;; for contracted in-query macro in db/base
         prepare
         start-transaction
         commit-transaction
         rollback-transaction
         call-with-transaction
         in-transaction?
         needs-rollback?
         list-tables
         table-exists?
         group-rows
         rows->dict)

;; == Administrative procedures

(define (connected? x)
  (send x connected?))

(define (disconnect x)
  (send x disconnect))

(define (connection-dbsystem x)
  (send x get-dbsystem))

(define (dbsystem-name x)
  (send x get-short-name))

(define (dbsystem-supported-types x)
  ;; FIXME: make version sensitive?
  (send x get-known-types +inf.0))

;; == Misc procedures

;; Value of prop:statement should be a function from struct instance to statement.
(define-values (prop:statement prop:statement? prop:statement-ref)
  (make-struct-type-property 'prop:statement))

(define (statement? x)
  (or (string? x)
      (prepared-statement? x)
      (statement-binding? x)
      (prop:statement? x)))

(define (bind-prepared-statement pst params)
  (send pst bind 'bind-prepared-statement params))

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
          (let* ([table (virtual-statement-table stmt)]
                 [gen (virtual-statement-gen stmt)]
                 [base-c (send c get-base)])
            (let ([table-pst (and base-c (hash-ref table base-c #f))])
              (or table-pst
                  (let* ([sql-string (gen (send c get-dbsystem))]
                         ;; FIXME: virtual-connection:prepare1 handles
                         ;; fsym = 'virtual-statement case specially
                         [pst (prepare1 'virtual-statement c sql-string #f)])
                    (hash-set! table base-c pst)
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
  (send c query fsym stmt #f))

;; query/rows : connection symbol Statement nat/#f -> rows-result
(define (query/rows c fsym sql want-columns)
  (let [(result (query1 c fsym sql))]
    (unless (rows-result? result)
      (error/want-rows fsym sql #t))
    (let ([got-columns (length (rows-result-headers result))])
      (when (and want-columns (not (= got-columns want-columns)))
        (error/column-count fsym sql want-columns got-columns #t)))
    result))

(define (query/cursor c fsym sql want-columns)
  (let ([result (send c query fsym sql #t)])
    (unless (cursor-result? result)
      (error/want-cursor fsym sql))
    (let ([got-columns (length (cursor-result-headers result))])
      (when (and want-columns (not (= got-columns want-columns)))
        (error/column-count fsym sql want-columns got-columns #t)))
    result))

(define (rows-result->row fsym rs sql maybe-row? one-column?)
  (define rows (rows-result-rows rs))
  (cond [(null? rows)
         (cond [maybe-row? #f]
               [else (error/row-count fsym sql 1 0)])]
        [(null? (cdr rows))
         (let ([row (car rows)])
           (cond [one-column? (vector-ref row 0)]
                 [else row]))]
        [else (error/row-count fsym sql 1 (length rows))]))

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
                       (error/statement-binding-args fsym stmt args)])])
           (send pst check-results fsym checktype stmt)
           (send pst bind fsym args))]
        [else ;; no args, and stmt is either string or statement-binding
         stmt]))

;; Query API procedures

;; query-rows : connection Statement arg ... -> (listof (vectorof 'a))
(define (query-rows c sql
                    #:group [group-fields-list null]
                    #:group-mode [group-mode null]
                    . args)
  (let* ([sql (compose-statement 'query-rows c sql args 'rows)]
         [result (query/rows c 'query-rows sql #f)]
         [result
          (cond [(not (null? group-fields-list))
                 (group-rows-result* 'query-rows result group-fields-list group-mode)]
                [else result])])
    (rows-result-rows result)))

;; query-list : connection Statement arg ... -> (listof 'a)
;; Expects to get back a rows-result with one field per row.
(define (query-list c sql . args)
  (let ([sql (compose-statement 'query-list c sql args 1)])
    (map (lambda (v) (vector-ref v 0))
         (rows-result-rows (query/rows c 'query-list sql 1)))))

;; query-row : connection Statement arg ... -> (vector-of 'a)
;; Expects to get back a rows-result of zero or one rows.
(define (query-row c sql . args)
  (let ([sql (compose-statement 'query-row c sql args 'rows)])
    (rows-result->row 'query-row
                    (query/rows c 'query-row sql #f)
                    sql #f #f)))

;; query-maybe-row : connection Statement arg ... -> (vector-of 'a) or #f
;; Expects to get back a rows-result of zero or one rows.
(define (query-maybe-row c sql . args)
  (let ([sql (compose-statement 'query-maybe-row c sql args 'rows)])
    (rows-result->row 'query-maybe-row
                    (query/rows c 'query-maybe-row sql #f)
                    sql #t #f)))

;; query-value : connection string arg ... -> value | raises error
;; Expects to get back a rows-result of exactly one row, exactly one column.
(define (query-value c sql . args)
  (let ([sql (compose-statement 'query-value c sql args 1)])
    (rows-result->row 'query-value
                    (query/rows c 'query-value sql 1)
                    sql #f #t)))

;; query-maybe-value : connection Statement arg ... -> value/#f
;; Expects to get back a rows-result of zero or one rows, exactly one column.
(define (query-maybe-value c sql . args)
  (let ([sql (compose-statement 'query-maybe-value c sql args 1)])
    (rows-result->row 'query-maybe-value
                    (query/rows c 'query-maybe-value sql 1)
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

(define (in-query c stmt
                  #:fetch [fetch-size +inf.0]
                  #:group [grouping-fields null]
                  #:group-mode [group-mode null]
                  . args)
  (apply in-query-helper #f c stmt
         #:fetch fetch-size
         #:group grouping-fields
         #:group-mode group-mode
         args))

(define (in-query-helper vars c stmt
                         #:fetch [fetch-size +inf.0]
                         #:group [grouping-fields null]
                         #:group-mode [group-mode null]
                         . args)
  (when (and (not (null? grouping-fields))
             (< fetch-size +inf.0))
    (error 'in-query "cannot apply grouping to cursor (finite fetch-size)"))
  (let* ([check
          ;; If grouping, can't check expected arity.
          ;; FIXME: should check header includes named fields
          (cond [(null? grouping-fields) (or vars 'rows)]
                [else 'rows])]
         [stmt (compose-statement 'in-query c stmt args check)])
    (cond [(eqv? fetch-size +inf.0)
           (in-list/vector->values
            (rows-result-rows
             (let ([result (query/rows c 'in-query stmt vars)])
               (if (null? grouping-fields)
                   result
                   (group-rows-result* 'in-query result grouping-fields group-mode)))))]
          [else
           (let ([cursor (query/cursor c 'in-query stmt vars)])
             (in-list-generator/vector->values
              (lambda () (send c fetch/cursor 'in-query cursor fetch-size))))])))

(define (in-list/vector->values vs)
  (make-do-sequence
   (lambda ()
     (values (lambda (p) (vector->values (car p)))
             cdr
             vs
             pair? #f #f))))

(define (in-list-generator/vector->values fetch-proc)
  ;; fetch-proc : symbol nat -> (U list #f)
  ;; state = #f | (cons vector (U state (promise-of state)))

  ;; more-promise : -> (promise-of state)
  (define (more-promise)
    (delay (let ([more (fetch-proc)])
             ;; note: improper append, list onto promise
             (and more (append more (more-promise))))))

  (make-do-sequence
   (lambda ()
     (values (lambda (p) (vector->values (car p)))
             (lambda (p)
               (let ([next (cdr p)]) (if (promise? next) (force next) next)))
             (force (more-promise))
             pair? #f #f))))

;; ========================================

(define (prepare c stmt)
  ;; FIXME: handle non-string statements
  (prepare1 'prepare c stmt #f))

;; ----

(define (prepare1 fsym c stmt close-on-exec?)
  ;; stmt is string
  (send c prepare fsym stmt close-on-exec?))

;; ========================================

(define (start-transaction c
                           #:isolation [isolation #f]
                           #:option [option #f])
  (send c start-transaction 'start-transaction isolation option #f))

(define (commit-transaction c)
  (send c end-transaction 'commit-transaction 'commit #f))

(define (rollback-transaction c)
  (send c end-transaction 'rollback-transaction 'rollback #f))

(define (call-with-transaction c proc
                               #:isolation [isolation #f]
                               #:option [option #f])
  (send c start-transaction '|call-with-transaction (start)| isolation option #t)
  (with-handlers ([exn?
                   (lambda (e1)
                     (with-handlers ([exn?
                                      (lambda (e2)
                                        (error/exn-in-rollback 'call-with-transaction e1 e2))])
                       (when (send c connected?)
                         (send c end-transaction '|call-with-transaction (rollback)| 'rollback #t)))
                     (raise e1))])
    (begin0 (call-with-continuation-barrier proc)
      (send c end-transaction '|call-with-transaction (commit)| 'commit #t))))

(define (in-transaction? c)
  (and (send c transaction-status 'in-transaction?) #t))

(define (needs-rollback? c)
  (eq? (send c transaction-status 'needs-rollback?) 'invalid))

;; ========================================

;; list-tables : ... -> (listof string)
;;  - lists unqualified table/view/etc names in search path (omit system tables, if possible).
;;    Maybe it would be better to just search the current schema only?
;;    or maybe mode = 'current | 'search | 'current-or-search (default)
;;  - lists unqualified table/view/etc names for given schema (and/or catalog?)
;;  - Add option to include system tables?
(define (list-tables c
                     #:schema [schema 'search-or-current])
  (send c list-tables 'list-tables schema))

(define (table-exists? c table-name
                       #:schema [schema 'search-or-current]
                       #:case-sensitive? [cs? #f])
  (let ([tables (send c list-tables 'table-exists? schema)])
    (for/or ([table (in-list tables)])
      (if cs?
          (string=? table-name table)
          (string-ci=? table-name table)))))

;; list-tables* : ... -> (listof vector)
;; Return full catalog/schema/table/type list.

;; ========================================

;; FIXME: add 'assume-sorted optimization option?

(define (group-rows result
                    #:group key-fields-list
                    #:group-mode [group-mode null])
  (when (null? key-fields-list)
    (error 'group-rows "expected at least one grouping field set"))
  (group-rows-result* 'group-rows result key-fields-list group-mode))

(define (group-rows-result* fsym result key-fields-list group-mode)
  (let* ([invert-outer? (not (or (memq 'preserve-null group-mode)
                                 ;; old flag, deprecated:
                                 (memq 'preserve-null-rows group-mode)))]
         [as-list? (memq 'list group-mode)]
         [headers (rows-result-headers result)]
         [total-fields (length headers)]
         [name-map (headers->name-map headers)]
         [fields-used (make-vector total-fields #f)]
         [key-indexes-list
          (group-list->indexes fsym name-map total-fields fields-used key-fields-list)]
         [residual-length
          (for/sum ([x (in-vector fields-used)]) (if x 0 1))])
    (when (= residual-length 0)
      (raise-arguments-error fsym "cannot group by all fields"
                             "grouping field sets" key-fields-list))
    (when (and (> residual-length 1) as-list?)
      (raise-arguments-error fsym "expected exactly one residual field when #:group-mode is 'list"
                             "grouping field sets" key-fields-list
                             "residual field count" residual-length))
    (let* ([initial-projection
            (for/vector #:length total-fields ([i (in-range total-fields)]) i)]
           [headers
            (group-headers (list->vector headers)
                           initial-projection
                           key-indexes-list)]
           [rows
            (group-rows* fsym
                         (rows-result-rows result)
                         initial-projection
                         key-indexes-list
                         invert-outer?
                         as-list?)])
      (rows-result headers rows))))

(define (headers->name-map headers)
  (for/hash ([header (in-list headers)]
             [i (in-naturals)]
             #:when (assq 'name header))
    (values (cdr (assq 'name header)) i)))

(define (group-list->indexes fsym name-map total-fields fields-used key-fields-list)
  (let ([key-fields-list (if (list? key-fields-list) key-fields-list (list key-fields-list))])
    (for/list ([key-fields (in-list key-fields-list)])
      (group->indexes fsym name-map total-fields fields-used key-fields))))

(define (group->indexes fsym name-map total-fields fields-used key-fields)
  (let ([key-fields (if (vector? key-fields) key-fields (vector key-fields))])
    (for/vector ([key-field (in-vector key-fields)])
      (grouping-field->index fsym name-map total-fields fields-used key-field))))

(define (grouping-field->index fsym name-map total-fields fields-used key-field)
  (let ([key-index
         (cond [(string? key-field)
                (hash-ref name-map key-field #f)]
               [else key-field])])
    (when (string? key-field)
      (unless key-index
        (raise-arguments-error fsym "bad grouping field"
                               "given" key-field
                               "available" (sort (hash-keys name-map) string<?))))
    (when (exact-integer? key-field)
      (unless (< key-index total-fields)
        (raise-range-error fsym "fields" "grouping "
                           key-index
                           (sort (hash-keys name-map) string<?)
                           0 total-fields)))
    (when fields-used
      (when (vector-ref fields-used key-index)
        (raise-arguments-error fsym "grouping field used multiple times"
                               "field" key-field))
      (vector-set! fields-used key-index #t))
    key-index))

(define (group-headers headers projection key-indexes-list)
  (define (get-headers vec)
    (for/list ([index (in-vector vec)])
      (vector-ref headers index)))
  (cond [(null? key-indexes-list)
         (get-headers projection)]
        [else
         (let* ([key-indexes (car key-indexes-list)]
                [residual-projection
                 (vector-filter-not (lambda (index) (vector-member index key-indexes))
                                    projection)]
                [residual-headers
                 (group-headers headers residual-projection (cdr key-indexes-list))])
           (append (get-headers key-indexes)
                   (list `((name . "grouped") (grouped . ,residual-headers)))))]))

(define (group-rows* fsym rows projection key-indexes-list invert-outer? as-list?)
  ;; projection is vector of indexes (actually projection and permutation)
  ;; invert-outer? => residual rows with all NULL fields are dropped.
  (cond [(null? key-indexes-list)
         ;; Apply projection to each row
         (cond [as-list?
                (unless (= (vector-length projection) 1)
                  (error/internal
                   fsym 
                   "list mode requires a single residual column, got ~s"
                   (vector-length projection)))
                (let ([index (vector-ref projection 0)])
                  (for/list ([row (in-list rows)])
                    (vector-ref row index)))]
               [else
                (let ([plen (vector-length projection)])
                  (for/list ([row (in-list rows)])
                    (let ([v (make-vector plen)])
                      (for ([i (in-range plen)])
                        (vector-set! v i (vector-ref row (vector-ref projection i))))
                      v)))])]
        [else
         (let ()
           (define key-indexes (car key-indexes-list))
           (define residual-projection
             (vector-filter-not (lambda (index) (vector-member index key-indexes))
                                projection))
           (define key-row-length (vector-length key-indexes))
           (define (row->key-row row)
             (for/vector #:length key-row-length
                         ([i (in-vector key-indexes)])
                         (vector-ref row i)))
           (define (residual-all-null? row)
             (for/and ([i (in-vector residual-projection)])
                      (sql-null? (vector-ref row i))))
           (let* ([key-table (make-hash)]
                  [r-keys
                   (for/fold ([r-keys null])
                       ([row (in-list rows)])
                     (let* ([key-row (row->key-row row)]
                            [already-seen? (and (hash-ref key-table key-row #f) #t)])
                       (unless already-seen?
                         (hash-set! key-table key-row null))
                       (unless (and invert-outer? (residual-all-null? row))
                         (hash-set! key-table key-row (cons row (hash-ref key-table key-row))))
                       (if already-seen?
                           r-keys
                           (cons key-row r-keys))))])
             (for/list ([key (in-list (reverse r-keys))])
               (let ([residuals
                      (group-rows* fsym
                                   (reverse (hash-ref key-table key))
                                   residual-projection
                                   (cdr key-indexes-list)
                                   invert-outer?
                                   as-list?)])
                 (vector-append key (vector residuals))))))]))

;; ========================================

(define not-given (gensym 'not-given))

(define (rows->dict result
                    #:key key-field/s
                    #:value value-field/s
                    #:value-mode [value-mode null])
  (let* ([who 'rows->dict]
         [headers (rows-result-headers result)]
         [total-fields (length headers)]
         [name-map (headers->name-map headers)]
         [preserve-null? (memq 'preserve-null value-mode)]
         [value-list? (memq 'list value-mode)])
    (define (make-project field/s)
      (if (vector? field/s)
          (let* ([indexes (group->indexes who name-map total-fields #f field/s)]
                 [indexes-length (vector-length indexes)])
            (lambda (v)
              (for/vector #:length indexes-length ([i (in-vector indexes)])
                (vector-ref v i))))
          (let ([index (grouping-field->index who name-map total-fields #f field/s)])
            (lambda (v) (vector-ref v index)))))
    (define get-key (make-project key-field/s))
    (define get-value (make-project value-field/s))
    (define ok-value?
      (cond [preserve-null? (lambda (v) #t)]
            [(vector? value-field/s)
             (lambda (v) (not (for/or ([e (in-vector v)]) (sql-null? e))))]
            [else (lambda (v) (not (sql-null? v)))]))
    (for/fold ([table '#hash()]) ([row (in-list (if value-list?
                                                    (reverse (rows-result-rows result))
                                                    (rows-result-rows result)))])
      (let* ([key (get-key row)]
             [value (get-value row)]
             [old-value (hash-ref table key (if value-list? '() not-given))])
        (unless (or value-list?
                    (eq? (hash-ref table key not-given) not-given)
                    ;; FIXME: okay to coalesce values if equal?
                    (equal? value old-value))
          (error* who "duplicate value for key"
                  '("key" value) key
                  '("values" multi value) (list old-value value)))
        (if value-list?
            (hash-set table key
                      (if (ok-value? value)
                          (cons value old-value)
                          ;; If all-NULL value, still enter key => '() into dict
                          old-value))
            (if (ok-value? value)
                (hash-set table key value)
                table))))))
