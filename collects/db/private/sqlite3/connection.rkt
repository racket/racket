#lang racket/base
(require racket/class
         ffi/unsafe
         "../generic/interfaces.rkt"
         "../generic/prepared.rkt"
         "../generic/sql-data.rkt"
         "ffi.rkt"
         "dbsystem.rkt")
(provide connection%
         handle-status*)

;; == Connection

(define connection%
  (class* transactions% (connection<%>)
    (init db)
    (init-private busy-retry-limit
                  busy-retry-delay)

    (define -db db)
    (define statement-table (make-weak-hasheq))
    (define saved-tx-status #f) ;; set by with-lock, only valid while locked

    (inherit call-with-lock*
             add-delayed-call!
             check-valid-tx-status)
    (inherit-field tx-status)  ;; only #f or 'invalid for compat w/ check-valid-tx-status

    (define/override (call-with-lock fsym proc)
      (call-with-lock* fsym (lambda () (set! saved-tx-status (get-tx-status)) (proc)) #f #t))

    (define/private (get-db fsym)
      (or -db (error/not-connected fsym)))

    (define/public (get-dbsystem) dbsystem)
    (define/override (connected?) (and -db #t))

    (define/public (query fsym stmt)
      (let-values ([(stmt* info rows)
                    (call-with-lock fsym
                      (lambda ()
                        (check-valid-tx-status fsym)
                        (query1 fsym stmt)))])
        (statement:after-exec stmt)
        (cond [(pair? info) (recordset info rows)]
              [else (simple-result '())])))

    (define/private (query1 fsym stmt)
      (let* ([stmt (cond [(string? stmt)
                          (let* ([pst (prepare1 fsym stmt #t)])
                            (send pst bind fsym null))]
                         [(statement-binding? stmt)
                          stmt])]
             [pst (statement-binding-pst stmt)]
             [params (statement-binding-params stmt)])
        (send pst check-owner fsym this stmt)
        (let ([db (get-db fsym)]
              [stmt (send pst get-handle)])
          (HANDLE fsym (sqlite3_reset stmt))
          (HANDLE fsym (sqlite3_clear_bindings stmt))
          (for ([i (in-naturals 1)]
                [param (in-list params)])
            (load-param fsym db stmt i param))
          (let* ([info
                  (for/list ([i (in-range (sqlite3_column_count stmt))])
                    `((name ,(sqlite3_column_name stmt i))
                      (decltype ,(sqlite3_column_decltype stmt i))))]
                 [rows (step* fsym db stmt)])
            (HANDLE fsym (sqlite3_reset stmt))
            (HANDLE fsym (sqlite3_clear_bindings stmt))
            (values stmt info rows)))))

    (define/private (load-param fsym db stmt i param)
      (HANDLE fsym
       (cond [(int64? param)
              (sqlite3_bind_int64 stmt i param)]
             [(real? param) ;; includes >64-bit exact integers
              (sqlite3_bind_double stmt i (exact->inexact param))]
             [(string? param)
              (sqlite3_bind_text stmt i param)]
             [(bytes? param)
              (sqlite3_bind_blob stmt i param)]
             [(sql-null? param)
              (sqlite3_bind_null stmt i)]
             [else
              (error/internal fsym "bad parameter: ~e" param)])))

    (define/private (step* fsym db stmt)
      (let ([c (step fsym db stmt)])
        (if c (cons c (step* fsym db stmt)) null)))

    (define/private (step fsym db stmt)
      (let ([s (HANDLE fsym (sqlite3_step stmt))])
        (cond [(= s SQLITE_DONE) #f]
              [(= s SQLITE_ROW)
               (let* ([column-count (sqlite3_column_count stmt)]
                      [vec (make-vector column-count)])
                 (for ([i (in-range column-count)])
                   (vector-set! vec i
                                (let ([type (sqlite3_column_type stmt i)])
                                  (cond [(= type SQLITE_NULL)
                                         sql-null]
                                        [(= type SQLITE_INTEGER)
                                         (sqlite3_column_int64 stmt i)]
                                        [(= type SQLITE_FLOAT)
                                         (sqlite3_column_double stmt i)]
                                        [(= type SQLITE_TEXT)
                                         (sqlite3_column_text stmt i)]
                                        [(= type SQLITE_BLOB)
                                         (sqlite3_column_blob stmt i)]
                                        [else
                                         (error/internal
                                          fsym "unknown column type: ~e" type)]))))
                 vec)])))

    (define/public (prepare fsym stmt close-on-exec?)
      (call-with-lock fsym
        (lambda ()
          (check-valid-tx-status fsym)
          (prepare1 fsym stmt close-on-exec?))))

    (define/private (prepare1 fsym sql close-on-exec?)
      ;; no time between sqlite3_prepare and table entry
      (let*-values ([(db) (get-db fsym)]
                    [(prep-status stmt)
                     (HANDLE fsym
                      (let-values ([(prep-status stmt tail)
                                    (sqlite3_prepare_v2 db sql)])
                        (define (free!) (when stmt (sqlite3_finalize stmt)))
                        (when (string=? sql tail)
                          (free!) (uerror fsym "SQL syntax error in ~e" tail))
                        (when (not (zero? (string-length tail)))
                          (free!) (uerror fsym "multiple SQL statements given: ~e" tail))
                        (values prep-status stmt)))])
        (unless stmt (error/internal fsym "prepare failed"))
        (let* ([param-typeids
                (for/list ([i (in-range (sqlite3_bind_parameter_count stmt))])
                  'any)]
               [result-dvecs
                (for/list ([i (in-range (sqlite3_column_count stmt))])
                  '#(any))]
               [pst (new prepared-statement%
                         (handle stmt)
                         (close-on-exec? close-on-exec?)
                         (param-typeids param-typeids)
                         (result-dvecs result-dvecs)
                         (owner this))])
          (hash-set! statement-table pst #t)
          pst)))

    (define/public (disconnect)
      ;; FIXME: Reorder effects to be more robust if thread killed within disconnect (?)
      (define (go)
        (when -db
          (let ([db -db]
                [statements (hash-map statement-table (lambda (k v) k))])
            (set! -db #f)
            (set! statement-table #f)
            (for ([pst (in-list statements)])
              (let ([stmt (send pst get-handle)])
                (when stmt
                  (send pst set-handle #f)
                  (HANDLE 'disconnect (sqlite3_finalize stmt)))))
            (HANDLE 'disconnect (sqlite3_close db))
            (void))))
      (call-with-lock* 'disconnect go go #f))

    (define/public (free-statement pst)
      (define (go)
        (let ([stmt (send pst get-handle)])
          (when stmt
            (send pst set-handle #f)
            (HANDLE 'free-statement (sqlite3_finalize stmt))
            (void))))
      (call-with-lock* 'free-statement go go #f))


    ;; == Transactions

    ;; http://www.sqlite.org/lang_transaction.html

    (define/public (transaction-status fsym)
      (call-with-lock fsym
        (lambda ()
          (let ([db (get-db fsym)])
            (or tx-status (get-tx-status db))))))

    (define/private (get-tx-status [db -db])
      (and db (not (sqlite3_get_autocommit db))))

    (define/public (start-transaction fsym isolation)
      ;; Isolation level can be set to READ UNCOMMITTED via pragma, but
      ;; ignored in all but a few cases, don't bother.
      ;; FIXME: modes are DEFERRED | IMMEDIATE | EXCLUSIVE
      (let ([stmt
             (call-with-lock fsym
               (lambda ()
                 (let ([db (get-db fsym)])
                   (when (get-tx-status db)
                     (error/already-in-tx fsym))
                   (let-values ([(stmt* _info _rows)
                                 (query1 fsym "BEGIN TRANSACTION")])
                     stmt*))))])
        (statement:after-exec stmt)
        (void)))

    (define/public (end-transaction fsym mode)
      (let ([stmt
             (call-with-lock fsym
               (lambda ()
                 (let ([db (get-db fsym)])
                   (unless (eq? mode 'rollback)
                     (check-valid-tx-status fsym))
                   (when (get-tx-status db)
                     (let-values ([(stmt* _info _rows)
                                   (case mode
                                     ((commit)
                                      (query1 fsym "COMMIT TRANSACTION"))
                                     ((rollback)
                                      (query1 fsym "ROLLBACK TRANSACTION")))])
                       (set! tx-status #f)
                       stmt*)))))])
        (statement:after-exec stmt)
        (void)))

    ;; ----

    (define-syntax-rule (HANDLE who expr)
      (handle* who (lambda () expr) 0))

    (define/private (handle* who thunk iteration)
      (call-with-values thunk
        (lambda (s . rest)
          (cond [(and (= s SQLITE_BUSY) (< iteration busy-retry-limit))
                 (dbdebug "sqlite: busy, will retry")
                 (sleep busy-retry-delay)
                 (handle* who thunk (add1 iteration))]
                [else (apply values (handle-status who s) rest)]))))

    ;; Some errors can cause whole transaction to rollback;
    ;; (see http://www.sqlite.org/lang_transaction.html)
    ;; So when those errors occur, compare current tx-status w/ saved.
    ;; Can't figure out how to test...
    (define/private (handle-status who s)
      (when (memv s maybe-rollback-status-list)
        (when (and saved-tx-status (not (get-tx-status -db))) ;; was in trans, now not
          (set! tx-status 'invalid)))
      (handle-status* who s -db))

    ;; ----

    (super-new)
    (register-finalizer this (lambda (obj) (send obj disconnect)))))

;; ----------------------------------------

;; handle-status : symbol integer -> integer
;; Returns the status code if no error occurred, otherwise
;; raises an exception with an appropriate message.
(define (handle-status* who s db)
  (if (or (= s SQLITE_OK)
          (= s SQLITE_ROW)
          (= s SQLITE_DONE))
      s
      (uerror who "~a" (lookup-status-message s db))))

(define error-table
  `([,SQLITE_ERROR . "unknown error"]
    [,SQLITE_INTERNAL . "an internal logic error in SQLite"]
    [,SQLITE_PERM . "access permission denied"]
    [,SQLITE_ABORT . "callback routine requested an abort"]
    [,SQLITE_BUSY . "the database file is locked"]
    [,SQLITE_LOCKED . "table in the database is locked"]
    [,SQLITE_NOMEM . "malloc() failed"]
    [,SQLITE_READONLY . "attempt to write a readonly database"]
    [,SQLITE_INTERRUPT . "operation terminated by sqlite3_interrupt()"]
    [,SQLITE_IOERR . "some kind of disk I/O error occurred"]
    [,SQLITE_CORRUPT . "the database disk image is malformed"]
    [,SQLITE_NOTFOUND . "(internal only) table or record not found"]
    [,SQLITE_FULL . "insertion failed because database is full"]
    [,SQLITE_CANTOPEN . "unable to open the database file"]
    [,SQLITE_PROTOCOL . "database lock protocol error"]
    [,SQLITE_EMPTY . "database is empty"]
    [,SQLITE_SCHEMA . "database schema changed"]
    [,SQLITE_TOOBIG . "too much data for one row of a table"]
    [,SQLITE_CONSTRAINT . "abort due to contraint violation"]
    [,SQLITE_MISMATCH . "data type mismatch"]
    [,SQLITE_MISUSE . "library used incorrectly"]
    [,SQLITE_NOLFS . "uses OS features not supported on host"]
    [,SQLITE_AUTH . "authorization denied"]
    [,SQLITE_FORMAT . "auxiliary database format error"]
    [,SQLITE_RANGE . "2nd parameter to sqlite3_bind out of range"]
    [,SQLITE_NOTADB . "file opened that is not a database file"]))

;; lookup-status-message : integer db/#f -> string
(define (lookup-status-message s db)
  (cond [(and (eq? s SQLITE_ERROR) db)
         (sqlite3_errmsg db)]
        [(assoc s error-table) => cdr]
        [else "unknown condition"]))

;; http://www.sqlite.org/lang_transaction.html
(define maybe-rollback-status-list
  (list SQLITE_FULL SQLITE_IOERR SQLITE_BUSY SQLITE_NOMEM SQLITE_INTERRUPT))
