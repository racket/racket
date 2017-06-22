#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/atomic
         ffi/unsafe/custodian
         "../generic/interfaces.rkt"
         "../generic/common.rkt"
         "../generic/prepared.rkt"
         "../generic/sql-data.rkt"
         "ffi.rkt"
         "dbsystem.rkt")
(provide connection%
         handle-status*)

;; == Connection

(define connection%
  (class* statement-cache% (connection<%>)
    (init db)
    (init-private db-spec ;; #f or (list path mode)
                  busy-retry-limit
                  busy-retry-delay)

    (define -db db)
    (define saved-tx-status #f) ;; set by with-lock, only valid while locked

    (sqlite3_extended_result_codes db #t)

    ;; Must finalize all stmts before closing db, but also want stmts to be
    ;; independently finalizable. So db needs strong refs to stmts (but no
    ;; strong refs to prepared-statement% wrappers). Actually, sqlite3 maintains
    ;; stmt list internally, but sqlite3_next_stmt is not available on Mac OS
    ;; 10.5.* versions of libsqlite3.
    (define stmt-table (make-hasheq)) ;; hasheq[_sqlite3_statement => #t]

    (inherit call-with-lock*
             add-delayed-call!
             get-tx-status
             set-tx-status!
             check-valid-tx-status
             check-statement/tx
             dprintf
             prepare1
             check/invalidate-cache)
    (inherit-field DEBUG?)

    (define/override (call-with-lock fsym proc)
      (call-with-lock* fsym (lambda () (set! saved-tx-status (get-tx-status)) (proc)) #f #t))

    ;; Custodian shutdown can cause disconnect even in the middle of
    ;; operation (with lock held). So use (A _) around any FFI calls,
    ;; check still connected.
    (define-syntax-rule (A e ...)
      (call-as-atomic
       (lambda ()
         (unless -db (error/disconnect-in-lock 'sqlite3))
         e ...)))

    (define/private (get-db fsym)
      (or -db (error/not-connected fsym)))

    (define/public (get-dbsystem) dbsystem)
    (define/override (connected?) (and -db #t))

    (define/public (query fsym stmt cursor?)
      (call-with-lock fsym
        (lambda ()
          (check-valid-tx-status fsym)
          (query1 fsym stmt #t cursor?))))

    (define/private (query1 fsym stmt check-tx? cursor?)
      (let* ([stmt (check-statement fsym stmt cursor?)]
             [pst (statement-binding-pst stmt)]
             [params (statement-binding-params stmt)])
        (when check-tx? (check-statement/tx fsym (send pst get-stmt-type)))
        (let ([db (get-db fsym)]
              [delenda (check/invalidate-cache stmt)]
              [stmt (send pst get-handle)])
          (when DEBUG?
            (dprintf "  >> query statement #x~x with ~e\n" (cast stmt _pointer _uintptr) params))
          (when delenda
            (for ([pst (in-hash-values delenda)])
              (send pst finalize #f)))
          (A (sqlite3_reset stmt)
             (sqlite3_clear_bindings stmt))
          (for ([i (in-naturals 1)]
                [param (in-list params)])
            (load-param fsym db stmt i param))
          (let* ([info
                  (for/list ([i (in-range (A (sqlite3_column_count stmt)))])
                    (A `((name . ,(sqlite3_column_name stmt i))
                         (decltype . ,(sqlite3_column_decltype stmt i)))))]
                 [saved-last-insert-rowid
                  (and (null? info) (A (sqlite3_last_insert_rowid db)))]
                 [saved-total-changes
                  (and (null? info) (A (sqlite3_total_changes db)))]
                 [result
                  (or cursor?
                      (step* fsym db stmt #f +inf.0 pst))])
            (unless (eq? (get-tx-status) 'invalid)
              (set-tx-status! fsym (read-tx-status)))
            (unless cursor?
              (send pst after-exec #f))
            (cond [(and (pair? info) (not cursor?))
                   (rows-result info result)]
                  [(and (pair? info) cursor?)
                   (cursor-result info pst (box #f))]
                  [else
                   (simple-result
                    (let ([last-insert-rowid (A (sqlite3_last_insert_rowid db))]
                          [total-changes (A (sqlite3_total_changes db))])
                      ;; Not all statements clear last_insert_rowid, changes; so
                      ;; extra guards to make sure results are relevant.
                      `((insert-id
                         . ,(and (not (= last-insert-rowid saved-last-insert-rowid))
                                 last-insert-rowid))
                        (affected-rows
                         . ,(if (> total-changes saved-total-changes)
                                (A (sqlite3_changes db))
                                0)))))])))))

    (define/public (fetch/cursor fsym cursor fetch-size)
      (let ([pst (cursor-result-pst cursor)]
            [end-box (cursor-result-extra cursor)])
        (send pst check-owner fsym this pst)
        (call-with-lock fsym
          (lambda ()
            (cond [(unbox end-box) #f]
                  [else
                   (let ([stmt (send pst get-handle)])
                     (begin0 (step* fsym (get-db fsym) stmt end-box fetch-size pst)
                       (when (unbox end-box)
                         (send pst after-exec #f))))])))))

    (define/private (check-statement fsym stmt cursor?)
      (cond [(statement-binding? stmt)
             (let ([pst (statement-binding-pst stmt)])
               (send pst check-owner fsym this stmt)
               (cond [cursor?
                      (let ([pst* (prepare1 fsym (send pst get-stmt) #f)])
                        (statement-binding pst* (statement-binding-params stmt)))]
                     [else stmt]))]
            [(string? stmt)
             (let* ([pst (prepare1 fsym stmt (not cursor?))])
               (send pst bind fsym null))]))

    (define/private (load-param fsym db stmt i param)
      (HANDLE fsym
       (cond [(int64? param)
              (A (sqlite3_bind_int64 stmt i param))]
             [(real? param) ;; includes >64-bit exact integers
              (A (sqlite3_bind_double stmt i (exact->inexact param)))]
             [(string? param)
              (A (sqlite3_bind_text stmt i param))]
             [(bytes? param)
              (A (sqlite3_bind_blob stmt i param))]
             [(sql-null? param)
              (A (sqlite3_bind_null stmt i))]
             [else
              (error/internal* fsym "bad parameter value" '("value" value) param)])))

    (define/private (step* fsym db stmt end-box fetch-limit pst)
      (with-handlers ([exn:fail?
                       (lambda (e)
                         (A (sqlite3_reset stmt)
                            (sqlite3_clear_bindings stmt))
                         (raise e))])
        (let loop ([fetch-limit fetch-limit])
          (if (zero? fetch-limit)
              null
              (let ([c (step fsym db stmt pst)])
                (cond [c
                       (cons c (loop (sub1 fetch-limit)))]
                      [else
                       (A (sqlite3_reset stmt)
                          (sqlite3_clear_bindings stmt))
                       (when end-box (set-box! end-box #t))
                       null]))))))

    (define/private (step fsym db stmt pst)
      (let ([s (HANDLE fsym (A (sqlite3_step stmt)) pst)])
        (cond [(= s SQLITE_DONE) #f]
              [(= s SQLITE_ROW)
               (let* ([column-count (A (sqlite3_column_count stmt))]
                      [vec (make-vector column-count)])
                 (for ([i (in-range column-count)])
                   (vector-set! vec i
                                (let ([type (A (sqlite3_column_type stmt i))])
                                  (cond [(= type SQLITE_NULL)
                                         sql-null]
                                        [(= type SQLITE_INTEGER)
                                         (A (sqlite3_column_int64 stmt i))]
                                        [(= type SQLITE_FLOAT)
                                         (A (sqlite3_column_double stmt i))]
                                        [(= type SQLITE_TEXT)
                                         (A (sqlite3_column_text stmt i))]
                                        [(= type SQLITE_BLOB)
                                         (A (sqlite3_column_blob stmt i))]
                                        [else
                                         (error/internal* fsym "unknown column type"
                                                          "type" type)]))))
                 vec)])))

    (define/override (classify-stmt sql) (classify-sl-sql sql))

    (define/override (prepare1* fsym sql close-on-exec? stmt-type)
      (dprintf "  >> prepare ~e~a\n" sql (if close-on-exec? " close-on-exec" ""))
      (let*-values ([(db) (get-db fsym)]
                    [(prep-status stmt)
                     (HANDLE fsym
                       ;; Do not allow break/kill between prepare and
                       ;; entry of stmt in table.
                       (A (let-values ([(prep-status stmt tail?)
                                        (sqlite3_prepare_v2 db sql)])
                            (cond [(not (zero? prep-status))
                                   (when stmt (sqlite3_finalize stmt))
                                   (values prep-status #f)]
                                  [tail?
                                   (when stmt (sqlite3_finalize stmt))
                                   (error* fsym "multiple statements given"
                                           '("given" value) sql)]
                                  [else
                                   (when stmt (hash-set! stmt-table stmt #t))
                                   (values prep-status stmt)]))))])
        (when DEBUG?
          (dprintf "  << prepared statement #x~x\n" (cast stmt _pointer _uintptr)))
        (unless stmt (error* fsym "SQL syntax error" '("given" value) sql))
        (let* ([param-typeids
                (for/list ([i (in-range (A (sqlite3_bind_parameter_count stmt)))])
                  'any)]
               [result-dvecs
                (for/list ([i (in-range (A (sqlite3_column_count stmt)))])
                  '#(any))]
               [pst (new prepared-statement%
                         (handle stmt)
                         (close-on-exec? close-on-exec?)
                         (param-typeids param-typeids)
                         (result-dvecs result-dvecs)
                         (stmt-type stmt-type)
                         (stmt sql)
                         (owner this))])
          pst)))

    (define/override (disconnect* _politely?)
      (super disconnect* _politely?)
      (call-as-atomic
       (lambda ()
         (let ([db -db])
           (when db
             (set! -db #f)
             ;; Free all of connection's prepared statements. This will leave
             ;; pst objects with dangling foreign objects, so don't try to free
             ;; them again---check that -db is not-#f.
             (let ([stmts (hash-keys stmt-table)])
               (hash-clear! stmt-table)
               (for-each sqlite3_finalize stmts))
             (HANDLE 'disconnect (sqlite3_close db))
             (void))))))

    (define/public (get-base) this)

    (define/public (free-statement pst need-lock?)
      (define (go) (do-free-statement 'free-statement pst))
      (if need-lock?
          (call-with-lock* 'free-statement go go #f)
          (go)))

    (define/private (do-free-statement fsym pst)
      (call-as-atomic
       (lambda ()
         (let ([stmt (send pst get-handle)])
           (send pst set-handle #f)
           (when (and stmt -db)
             (hash-remove! stmt-table stmt)
             (sqlite3_finalize stmt))
           (void)))))

    ;; Internal query

    (define/private (internal-query1 fsym sql)
      (query1 fsym sql #f #f))

    ;; == Transactions

    ;; http://www.sqlite.org/lang_transaction.html

    (define/private (read-tx-status)
      (not (A (sqlite3_get_autocommit -db))))

    (define/override (start-transaction* fsym isolation option)
      ;; Isolation level can be set to READ UNCOMMITTED via pragma, but
      ;; ignored in all but a few cases, don't bother.
      ;; FIXME: modes are DEFERRED | IMMEDIATE | EXCLUSIVE
      (cond [(eq? isolation 'nested)
             (let ([savepoint (generate-name)])
               (internal-query1 fsym (format "SAVEPOINT ~a" savepoint))
               savepoint)]
            [else
             ;; Note: pragma read_uncommitted irrelevant, since we don't use
             ;; the shared page cache.
             (let ([sql
                    (case option
                      ((deferred #f) "BEGIN TRANSACTION")
                      ((immediate) "BEGIN IMMEDIATE TRANSACTION")
                      ((exclusive) "BEGIN EXCLUSIVE TRANSACTION")
                      (else (raise-argument-error fsym "(or/c 'deferred 'immediate 'exclusive #f)" option)))])
               (internal-query1 fsym sql)
               #f)]))

    (define/override (end-transaction* fsym mode savepoint)
      (case mode
        ((commit)
         (cond [savepoint
                (internal-query1 fsym (format "RELEASE SAVEPOINT ~a" savepoint))]
               [else
                (internal-query1 fsym "COMMIT TRANSACTION")]))
        ((rollback)
         (cond [savepoint
                ;; FIXME: if nested tx is invalid, enclosing tx might be invalid too
                ;; (eg, IOERR). Add way to communicate back enclosing tx validity.
                (internal-query1 fsym (format "ROLLBACK TO SAVEPOINT ~a" savepoint))
                (internal-query1 fsym (format "RELEASE SAVEPOINT ~a" savepoint))]
               [(read-tx-status)
                (internal-query1 fsym "ROLLBACK TRANSACTION")]
               [else
                ;; underlying tx already closed due to auto-rollback error
                (void)])
         ;; remove 'invalid status, if necessary
         (set-tx-status! fsym (read-tx-status))))
      (void))

    ;; name-counter : number
    (define name-counter 0)

    ;; generate-name : -> string
    (define/private (generate-name)
      (let ([n name-counter])
        (set! name-counter (add1 name-counter))
        (format "λmz_~a" n)))

    ;; Reflection

    (define/public (list-tables fsym schema)
      (let ([stmt
             ;; schema ignored, because sqlite doesn't support
             (string-append "SELECT tbl_name from sqlite_master "
                            "WHERE type = 'table' or type = 'view'")])
        (let ([result (call-with-lock fsym (lambda () (internal-query1 fsym stmt)))])
          (for/list ([row (in-list (rows-result-rows result))])
            (vector-ref row 0)))))

    ;; ----

    (define-syntax HANDLE
      (syntax-rules ()
        [(HANDLE who expr)
         (HANDLE who expr #f)]
        [(HANDLE who expr pst)
         (handle* who (lambda () expr) 0 pst)]))

    (define/private (handle* who thunk iteration pst)
      (call-with-values thunk
        (lambda (full-s . rest)
          (define s (simplify-status full-s))
          (cond [(and (= s SQLITE_BUSY) (< iteration busy-retry-limit))
                 (sleep busy-retry-delay)
                 (handle* who thunk (add1 iteration) pst)]
                [else
                 (when (> iteration 0)
                   (log-db-debug "continuing ~s with ~s after SQLITE_BUSY x ~s"
                                 who
                                 (if (= s SQLITE_BUSY) "SQLITE_BUSY" s)
                                 iteration))
                 (apply values (handle-status who full-s pst) rest)]))))

    ;; Some errors can cause whole transaction to rollback;
    ;; (see http://www.sqlite.org/lang_transaction.html)
    ;; So when those errors occur, compare current tx-status w/ saved.
    ;; Can't figure out how to test...
    (define/private (handle-status who full-s pst)
      (when (memv (simplify-status full-s) maybe-rollback-status-list)
        (when (and saved-tx-status -db (not (read-tx-status))) ;; was in trans, now not
          (set-tx-status! who 'invalid)))
      (handle-status* who full-s this db-spec pst))

    (define/public (get-error-message)
      (A (sqlite3_errmsg -db)))

    ;; ----
    (super-new)
    (register-finalizer-and-custodian-shutdown
     this
     ;; Keep a reference to the class to keep all FFI callout objects
     ;; (eg, sqlite3_close) used by its methods from being finalized.
     (let ([dont-gc this%])
       (lambda (obj)
         (send obj disconnect* #f)
         ;; Dummy result to prevent reference from being optimized away
         dont-gc)))))

;; ----------------------------------------

;; handle-status* : symbol integer [...] -> integer
;; Returns the status code if no error occurred, otherwise
;; raises an exception with an appropriate message.
(define (handle-status* who full-s db db-spec pst)
  (define s (simplify-status full-s))
  (define db-file (and db-spec (car db-spec)))
  (define db-mode (and db-spec (cadr db-spec)))
  (define sql (and pst (send pst get-stmt)))
  (cond [(or (= s SQLITE_OK)
             (= s SQLITE_ROW)
             (= s SQLITE_DONE))
         s]
        [else
         (let* ([info
                 (or (assoc s error-table)
                     '(#f unknown "unknown error code"))]
                [sym
                 (cadr info)]
                [message
                 (cond [(= s SQLITE_ERROR)
                        (cond [(is-a? db connection%)
                               (send db get-error-message)]
                              [(sqlite3_database? db)
                               (sqlite3_errmsg db)]
                              [else (caddr info)])]
                       [else (caddr info)])])
           (define extra
             (string-append
              ;; error code
              (format "\n  error code: ~s" full-s)
              ;; query, if available
              (cond [sql (format "\n  SQL: ~e" sql)]
                    [else ""])
              ;; db file and mode, if relevant and available
              (cond [(memv s include-db-file-status-list)
                     (string-append
                      (format "\n  database: ~e" (or db-file 'unknown))
                      (format "\n  mode: ~e" (or db-mode 'unknown))
                      (if (path-string? db-file)
                          (format "\n  file permissions: ~s"
                                  (file-or-directory-permissions db-file))
                          ""))]
                    [else ""])))
           (raise (make-exn:fail:sql (format "~a: ~a~a" who message extra)
                                     (current-continuation-marks)
                                     sym
                                     `((code . ,sym)
                                       (message . ,message)
                                       (errcode . ,full-s)
                                       (sql . ,sql)
                                       (db-file . ,db-file)
                                       (db-mode . ,db-mode)))))]))

(define (simplify-status s)
  (cond
   [(or (= SQLITE_IOERR_BLOCKED s)
        (= SQLITE_IOERR_LOCK s)
        (= SQLITE_READONLY_ROLLBACK s))
    ;; Kept in extended form, because these indicate
    ;; cases where retry is appropriate
    s]
   [else (bitwise-and s 255)]))

(define error-table
  `([,SQLITE_ERROR       error      "unknown error"]
    [,SQLITE_INTERNAL    internal   "an internal logic error in SQLite"]
    [,SQLITE_PERM        perm       "access permission denied"]
    [,SQLITE_ABORT       abort      "callback routine requested an abort"]
    [,SQLITE_BUSY        busy       "the database file is locked"]
    [,SQLITE_LOCKED      locked     "table in the database is locked"]
    [,SQLITE_NOMEM       nomem      "malloc() failed"]
    [,SQLITE_READONLY    readonly   "attempt to write a readonly database"]
    [,SQLITE_READONLY_ROLLBACK readonly-rollback "attempt to write a readonly database (hot journal)"]
    [,SQLITE_INTERRUPT   interrupt  "operation terminated by sqlite3_interrupt()"]
    [,SQLITE_IOERR       ioerr      "some kind of disk I/O error occurred"]
    [,SQLITE_IOERR_BLOCKED ioerr-blocked "some kind of disk I/O error occurred (blocked)"]
    [,SQLITE_IOERR_LOCK  ioerr-lock "some kind of disk I/O error occurred (lock)"]
    [,SQLITE_CORRUPT     corrupt    "the database disk image is malformed"]
    [,SQLITE_NOTFOUND    notfound   "(internal only) table or record not found"]
    [,SQLITE_FULL        full       "insertion failed because database is full"]
    [,SQLITE_CANTOPEN    cantopen   "unable to open the database file"]
    [,SQLITE_PROTOCOL    protocol   "database lock protocol error"]
    [,SQLITE_EMPTY       empty      "database is empty"]
    [,SQLITE_SCHEMA      schema     "database schema changed"]
    [,SQLITE_TOOBIG      toobig     "too much data for one row of a table"]
    [,SQLITE_CONSTRAINT  constraint "abort due to constraint violation"]
    [,SQLITE_MISMATCH    mismatch   "data type mismatch"]
    [,SQLITE_MISUSE      misuse     "library used incorrectly"]
    [,SQLITE_NOLFS       nolfs      "uses OS features not supported on host"]
    [,SQLITE_AUTH        auth       "authorization denied"]
    [,SQLITE_FORMAT      format     "auxiliary database format error"]
    [,SQLITE_RANGE       range      "2nd parameter to sqlite3_bind out of range"]
    [,SQLITE_NOTADB      notadb     "file opened that is not a database file"]))

;; http://www.sqlite.org/lang_transaction.html
(define maybe-rollback-status-list
  (list SQLITE_FULL SQLITE_IOERR SQLITE_BUSY SQLITE_NOMEM SQLITE_INTERRUPT
        SQLITE_IOERR_BLOCKED SQLITE_IOERR_LOCK))

(define include-db-file-status-list
  (list SQLITE_READONLY SQLITE_READONLY_ROLLBACK SQLITE_PERM SQLITE_ABORT SQLITE_BUSY SQLITE_LOCKED
        SQLITE_IOERR SQLITE_IOERR_BLOCKED SQLITE_IOERR_LOCK SQLITE_CORRUPT
        SQLITE_NOTFOUND SQLITE_FULL SQLITE_CANTOPEN SQLITE_PROTOCOL SQLITE_EMPTY
        SQLITE_FORMAT SQLITE_NOTADB))
