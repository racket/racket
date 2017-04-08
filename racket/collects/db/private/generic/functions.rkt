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
             (prepared-statement? stmt))
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

;; == Query API procedures

;; query-rows0 : connection Statement arg ... -> (listof (vectorof 'a))
(define (query-rows0 c sql . args)
  (let* ([sql (compose-statement 'query-rows c sql args 'rows)]
         [result (query/rows c 'query-rows sql #f)])
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
