#lang racket/base
(require racket/class
         ffi/file
         ffi/unsafe
         ffi/unsafe/atomic
         ffi/unsafe/custodian
         "../generic/interfaces.rkt"
         "../generic/common.rkt"
         "../generic/ffi-common.rkt"
         "../generic/prepared.rkt"
         "../generic/sql-data.rkt"
         "ffi.rkt"
         "dbsystem.rkt")
(provide connection%
         handle-status*
         (protect-out unsafe-load-extension
                      unsafe-create-function
                      unsafe-create-aggregate))

(define-local-member-name unsafe-load-extension)
(define-local-member-name unsafe-create-function)
(define-local-member-name unsafe-create-aggregate)

;; == Connection

(define connection%
  (class* (ffi-connection-mixin statement-cache%) (connection<%>)
    (init db)
    (init-private db-spec ;; #f or (list path mode)
                  busy-retry-limit
                  busy-retry-delay)
    (super-new)

    (define -db db)
    (define saved-tx-status #f) ;; set by with-lock, only valid while locked
    (define creg (register-custodian-shutdown this shutdown-connection #:ordered? #t))
    (register-finalizer this shutdown-connection)

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
             check/invalidate-cache
             get-use-os-thread?
             sync-call-in-os-thread)
    (inherit-field DEBUG?)

    (define/override (call-with-lock fsym proc)
      (call-with-lock* fsym (lambda () (set! saved-tx-status (get-tx-status)) (proc)) #f #t))

    ;; Custodian shutdown can cause disconnect even in the middle of
    ;; operation (with lock held). So use (A _) around any FFI calls,
    ;; check still connected.
    ;; Optimization: use faster {start,end}-atomic instead of call-as-atomic;
    ;; but must not raise exn within (A _)!
    (define-syntax-rule (A e ...)
      (begin (start-atomic)
             (unless -db (end-atomic) (error/disconnect-in-lock 'sqlite3))
             (begin0 (let () e ...) (end-atomic))))
    (define-syntax-rule (A* e ...)
      (begin (start-atomic) (begin0 (let () e ...) (end-atomic))))

    (define/private (get-db fsym)
      (or -db (error/not-connected fsym)))
    (define/override (-get-db) -db)

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
                  (and (null? info) (A (sqlite3_total_changes db)))])
            (define-values (result last-insert-rowid total-changes changes)
              (if cursor?
                  (values #t #f #f #f)
                  (get-result fsym stmt #f +inf.0 pst (not (pair? info)))))
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
                    (let ()
                      ;; Not all statements clear last_insert_rowid, changes; so
                      ;; extra guards to make sure results are relevant.
                      (define changes? (> total-changes saved-total-changes))
                      `((insert-id
                         ;; We want to report insert-id if statement was a *successful* INSERT,
                         ;; but we can't check that directly. Instead, check if either
                         ;; - last_insert_rowid changed (but this check might fail, if the inserted
                         ;;   row happens to have the same rowid was the last INSERT; for example,
                         ;;   because the last INSERT was to a different table); or
                         ;; - the statement looked like an INSERT and the db reports changes
                         ;;   (but this check misses WITH...INSERT statements).
                         ;; Note that we can't use the errno approach of setting last_insert_rowid
                         ;; to a known unused value, because there are no unused values (if an
                         ;; INTEGER PRIMARY KEY field exists, that is the rowid) and because the
                         ;; last_insert_rowid is visible to SQL statements.
                         . ,(and (or (not (= last-insert-rowid saved-last-insert-rowid))
                                     (and changes? (eq? (send pst get-stmt-type) 'insert)))
                                 last-insert-rowid))
                        (affected-rows . ,(if changes? changes 0)))))])))))

    (define/public (fetch/cursor fsym cursor fetch-size)
      (let ([pst (cursor-result-pst cursor)]
            [end-box (cursor-result-extra cursor)])
        (send pst check-owner fsym this pst)
        (call-with-lock fsym
          (lambda ()
            (cond [(unbox end-box) #f]
                  [else
                   (let ([stmt (send pst get-handle)])
                     (define-values (rows _lii _tc _c)
                       (get-result fsym stmt end-box fetch-size pst #f))
                     (when (unbox end-box)
                       (send pst after-exec #f))
                     rows)])))))

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

    (define/private (get-result who stmt end-box fetch-limit pst changes?)
      (with-handlers ([exn:fail?
                       (lambda (e)
                         (A* (when -db (sqlite3_reset stmt) (sqlite3_clear_bindings stmt)))
                         (raise e))])
        ((cond [(get-use-os-thread?)
                (define timeout (inexact->exact (ceiling (* 1000 busy-retry-delay))))
                (sync-call-in-os-thread
                 (lambda (db)
                   (sqlite3_busy_timeout db timeout)
                   (begin0 (get-result* who db stmt end-box fetch-limit pst changes?)
                     (sqlite3_busy_timeout db 0))))]
               [else (get-result* who #f stmt end-box fetch-limit pst changes?)]))))

    (define/private (get-result* who os-db stmt end-box fetch-limit pst changes?)
      ;; os-db is sqlite3_database if in OS thread, #f if in Racket thread
      (define (call-as-fine-atomic thunk) (if os-db (thunk) (A (thunk))))
      (define-syntax-rule (FA expr ...) (call-as-fine-atomic (lambda () expr ...)))
      ;; step* : -> (-> (values (U Vector (-> Error)) Int/#f Nat/#f Nat/#f))
      (define (step*)
        (let loop ([fetch-limit fetch-limit] [acc null])
          (cond [(zero? fetch-limit)
                 (return (reverse acc))]
                [(step)
                 => (lambda (c)
                      (cond [(procedure? c) c]
                            [else (loop (sub1 fetch-limit) (cons c acc))]))]
                [else
                 (FA (sqlite3_reset stmt) (sqlite3_clear_bindings stmt))
                 (when end-box (set-box! end-box #t))
                 (return (reverse acc))])))
      ;; step : -> (U #f Vector (-> Error))
      (define (step)
        (let loop ([iteration 0])
          (define full-s (FA (sqlite3_step stmt)))
          (define s (simplify-status full-s))
          (cond [(= s SQLITE_DONE) #f]
                [(= s SQLITE_ROW) (FA (-get-row who stmt))]
                [(and (= s SQLITE_BUSY) (< iteration busy-retry-limit))
                 ;; Normally, sleep and try again (cooperates w/ scheduler).
                 ;; In os-thread, can't sleep; see sqlite3_busy_timeout above.
                 (unless os-db (sleep busy-retry-delay))
                 (loop (add1 iteration))]
                [else (lambda () (handle-status who full-s pst))])))
      ;; return : X -> (-> (values X Int/#f Nat/#f Nat/#f))
      (define (return rows)
        (define-values (last-insert-rowid total-changes changes)
          (if changes?
              (FA (values (sqlite3_last_insert_rowid (or os-db -db))
                          (sqlite3_total_changes (or os-db -db))
                          (sqlite3_changes (or os-db -db))))
              (values #f #f #f)))
        (lambda () (values rows last-insert-rowid total-changes changes)))
      (step*))

    ;; -get-row : Symbol stmt -> (U Vector (-> Error))
    ;; PRE: in atomic mode
    (define/private (-get-row fsym stmt)
      (define column-count (sqlite3_column_count stmt))
      (define vec (make-vector column-count))
      (for ([i (in-range column-count)])
        (define val
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
                   (lambda ()
                     (error/internal* fsym "unknown column type" "type" type))])))
        (vector-set! vec i val))
      vec)

    (define/override (classify-stmt sql) (classify-sl-sql sql))

    (define/override (prepare1* fsym sql close-on-exec? stmt-type)
      (dprintf "  >> prepare ~e~a\n" sql (if close-on-exec? " close-on-exec" ""))
      (let*-values ([(db) (get-db fsym)]
                    [(prep-status stmt)
                     (HANDLE fsym
                       ;; Do not allow break/kill between prepare and
                       ;; entry of stmt in table.
                       ((A (let-values ([(prep-status stmt tail?)
                                         (sqlite3_prepare_v2 db sql)])
                             (cond [(not (zero? prep-status))
                                    (when stmt (sqlite3_finalize stmt))
                                    (lambda () (values prep-status #f))]
                                   [tail?
                                    (when stmt (sqlite3_finalize stmt))
                                    (lambda () ;; escape atomic mode (see A)
                                      (error fsym "multiple statements given\n  value: ~e" sql))]
                                   [else
                                    (when stmt (hash-set! stmt-table stmt #t))
                                    (lambda () (values prep-status stmt))])))))])
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

    (define/override (-get-do-disconnect) ;; PRE: atomic
      ;; Save and clear fields
      (define dont-gc this%)
      (define db -db)
      (define stmts (hash-keys stmt-table))
      (set! -db #f)
      (hash-clear! stmt-table)
      ;; Unregister custodian shutdown, unless called from custodian.
      (when creg (unregister-custodian-shutdown this creg))
      (set! creg #f)
      ;; Actually disconnect
      (lambda ()
        ;; Free all of connection's prepared statements. This will leave
        ;; pst objects with dangling foreign objects, so don't try to free
        ;; them again---check that -db is not-#f.
        (for-each sqlite3_finalize stmts)
        (HANDLE 'disconnect (sqlite3_close db))
        (void/reference-sink dont-gc)
        ;; FIXME: move handle here?
        void))

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
      ;; Allow this to be called after custodian-disconnect so that in-progress
      ;; query can complete.
      (not (A* (if -db (sqlite3_get_autocommit -db) #t))))

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

    ;; == Reflection

    (define/public (list-tables fsym schema)
      (let ([stmt
             ;; schema ignored, because sqlite doesn't support
             (string-append "SELECT tbl_name from sqlite_master "
                            "WHERE type = 'table' or type = 'view'")])
        (let ([result (call-with-lock fsym (lambda () (internal-query1 fsym stmt)))])
          (for/list ([row (in-list (rows-result-rows result))])
            (vector-ref row 0)))))

    ;; == Load Extension

    (define/public (unsafe-load-extension who lib)
      (define lib-path (cleanse-path (path->complete-path lib)))
      (security-guard-check-file who lib-path '(read execute))
      (call-with-lock who
        (lambda ()
          (HANDLE who (A (sqlite3_enable_load_extension -db 1)))
          (HANDLE who (A (sqlite3_load_extension -db lib-path)))
          (HANDLE who (A (sqlite3_enable_load_extension -db 0)))
          (void))))

    ;; == Create Function

    (define dont-gc null)

    (define/public (unsafe-create-function who name arity proc
                                           #:flags [flags null])
      (define wrapped (wrap-fun name proc))
      (call-with-lock who
       (lambda ()
         (set! dont-gc (cons wrapped dont-gc))
         (HANDLE who (A (sqlite3_create_function_v2/scalar
                         -db name (or arity -1) flags wrapped))))))

    (define/public (unsafe-create-aggregate who name arity step final [init #f]
                                            #:flags [flags null])
      (define aggbox (box init))
      (define wrapped-step (wrap-agg-step name step aggbox init))
      (define wrapped-final (wrap-agg-final name final aggbox init))
      (call-with-lock who
       (lambda ()
         (set! dont-gc (list* wrapped-step wrapped-final dont-gc))
         (HANDLE who
                 (A (sqlite3_create_function_v2/aggregate
                     -db name (or arity -1) flags wrapped-step wrapped-final))))))

    ;; == Error handling

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
    ))

(define shutdown-connection
  ;; Keep a reference to the class to keep all FFI callout objects
  ;; (eg, sqlite3_close) used by its methods from being finalized.
  (let ([dont-gc connection%])
    (lambda (obj)
      (send obj real-disconnect)
      ;; Dummy result to prevent reference from being optimized away
      dont-gc)))

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
