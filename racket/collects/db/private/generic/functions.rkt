#lang racket/base
(require racket/vector
         racket/class
         (only-in "prepared.rkt")
         "interfaces.rkt")
(provide (all-defined-out))

;; See also db-lib:db/private/generic/functions2

;; == Administrative procedures

(define (connected? x)
  (send x connected?))

(define (disconnect x)
  (send x disconnect))


;; == Statements

(define (statement? x)
  (or (string? x)
      (prepared-statement? x)
      (statement-binding? x)
      (prop:statement? x)))

;; prop:statement : property of (Self Connection -> Statement)
(define-values (prop:statement prop:statement? prop:statement-ref)
  (make-struct-type-property 'prop:statement))

;; A virtual-statement is:
;;   - (virtual-statement table gen)
;;     where table is a weak-hasheq[connection => prepared-statement]
;;     and gen is (dbsystem -> string)
(define-struct virtual-statement (table gen)
  #:omit-define-syntaxes
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

(define (virtual-statement gen)
  (make-virtual-statement (make-weak-hasheq)
                          (if (string? gen) (lambda (_) gen) gen)))


;; == Query helper procedures

;; ResultCheck = #f | 'rows | exact-positive-integer
;;   #f = no check, 'rows = want rows-result, n = want rows-result w/ n cols

;; query1 : Connection Symbol Statement -> QueryResult
(define (query1 c who stmt)
  (send c query who stmt #f))

;; query/rows : Connection Symbol Statement Nat/#f -> Rows-Result
(define (query/rows c who stmt want-columns)
  (check-rows-result who stmt want-columns (query1 c who stmt)))

;; check-rows-result : Symbol Statement ResultCheck Query-Result -> Rows-Result
(define (check-rows-result who sql want-columns result)
  (unless (rows-result? result)
    (error/want-rows who sql #t))
  (let ([got-columns (length (rows-result-headers result))])
    (when (and (exact-integer? want-columns) (not (= got-columns want-columns)))
      (error/column-count who sql want-columns got-columns #t)))
  result)

;; rows-result->row : Symbol Rows-Result Statement Boolean Boolean -> Vector/Any
(define (rows-result->row who result sql maybe-row? one-column?)
  (define rows (rows-result-rows result))
  (cond [(null? rows)
         (if maybe-row? #f (error/row-count who sql 1 0))]
        [(null? (cdr rows))
         (let ([row (car rows)])
           (if one-column? (vector-ref row 0) row))]
        [else (error/row-count who sql 1 (length rows))]))

;; compose-statement : Symbol Connection Statement List ResultCheck -> Statement
;; Returns self-contained statement: either string or statement-binding.
(define (compose-statement who c stmt args checktype)
  (cond [(prop:statement? stmt)
         (let ([stmt* ((prop:statement-ref stmt) stmt c)])
           (compose-statement who c stmt* args checktype))]
        [(or (pair? args) (prepared-statement? stmt))
         (define pst
           (cond [(string? stmt)
                  (prepare1 who c stmt #t)]
                 [(prepared-statement? stmt)
                  ;; Ownership check done later, by query method.
                  stmt]
                 [(statement-binding? stmt)
                  (error/statement-binding-args who stmt args)]))
         (send pst check-results who checktype stmt)
         (send pst bind who args)]
        [else ;; no args, and stmt is either string or statement-binding
         stmt]))

;; query-row* : Symbol Connection Statement List Nat/#f Boolean Boolean -> (varies)
;; Helper for all query operations that expect at most one row returned.
(define (query-row* who c sql args want-columns maybe-row? one-column?)
  (let* ([sql (compose-statement who c sql args (or want-columns 'rows))]
         [result (query/rows c who sql want-columns)])
    (rows-result->row who result sql maybe-row? one-column?)))


;; == Query API procedures

;; query-rows0 : connection Statement arg ... -> (listof (vectorof 'a))
(define (query-rows0 c sql . args)
  (let* ([sql (compose-statement 'query-rows c sql args 'rows)]
         [result (query/rows c 'query-rows0 sql #f)])
    (rows-result-rows result)))

;; query-list : connection Statement arg ... -> (listof 'a)
;; Expects to get back a rows-result with one field per row.
(define (query-list c sql . args)
  (let* ([sql (compose-statement 'query-list c sql args 1)]
         [result (query/rows c 'query-list sql 1)])
    (map (lambda (v) (vector-ref v 0)) (rows-result-rows result))))

;; query-row : Connection Statement SqlDatum ... -> (Vectorof SqlDatum)
(define (query-row c sql . args)
  (query-row* 'query-row c sql args #f #f #f))

;; query-maybe-row : Connection Statement SqlDatum ... -> (Vectorof SqlDatum) or #f
(define (query-maybe-row c sql . args)
  (query-row* 'query-maybe-row c sql args #f #t #f))

;; query-value : Connection Statement SqlDatum ... -> SqlDatum | raises error
(define (query-value c sql . args)
  (query-row* 'query-value c sql args 1 #f #t))

;; query-maybe-value : Connection Statement SqlDatum ... -> SqlDatum or #f
(define (query-maybe-value c sql . args)
  (query-row* 'query-maybe-value c sql args 1 #t #t))

;; query-exec : Connection Statement SqlDatum ... -> void
(define (query-exec c sql . args)
  (let ([sql (compose-statement 'query-exec c sql args #f)])
    (query1 c 'query-exec sql)
    (void)))

;; query : Connection Statement SqlDatum ... -> QueryResult
(define (query c sql . args)
  (let ([sql (compose-statement 'query c sql args #f)])
    (query1 c 'query sql)))


;; == Prepare

(define (prepare c stmt)
  ;; FIXME: handle non-string statements
  (prepare1 'prepare c stmt #f))

(define (prepare1 fsym c stmt close-on-exec?)
  ;; stmt is string
  (send c prepare fsym stmt close-on-exec?))


;; == Transactions

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
