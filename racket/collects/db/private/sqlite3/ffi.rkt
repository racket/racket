#lang racket/base
(require (for-syntax racket/base
                     setup/cross-system)
         racket/runtime-path
         racket/string
         ffi/unsafe
         ffi/unsafe/define
         setup/cross-system)
(require "ffi-constants.rkt")
(provide (all-from-out "ffi-constants.rkt")
         (protect-out (all-defined-out)))

;; raco distribute should include Racket's sqlite3 if present
(define-runtime-path sqlite-so
  #:runtime?-id runtime?
  (case (if runtime? (system-type) (cross-system-type))
    [(windows) '(so "sqlite3")]
    [else '(so "libsqlite3" ("0" #f))]))

(define sqlite-lib
  (case (system-type)
    [(windows) (ffi-lib sqlite-so #:fail (lambda () #f))]
    [else (ffi-lib sqlite-so '("0" #f) #:fail (lambda () #f))]))

(define-ffi-definer define-sqlite
  sqlite-lib
  #:default-make-fail make-not-available)

; Types
(define-cpointer-type _sqlite3_database)
(define-cpointer-type _sqlite3_statement)

;; -- Functions --

;; -- DB --

(define-sqlite sqlite3_libversion_number
  (_fun -> _int))

(define-sqlite sqlite3_open
  (_fun (filename ignored-flags) ::
        ((bytes-append filename #"\0") : _bytes)
        (db : (_ptr o _sqlite3_database))
        -> (result : _int)
        -> (values db result)))

(define-sqlite sqlite3_open_v2
  (_fun (filename flags) ::
        ((bytes-append filename #"\0") : _bytes)
        (db : (_ptr o _sqlite3_database))
        (flags : _int)
        (vfs : _pointer = #f)
        -> (result : _int)
        -> (values db result))
  #:fail (lambda () sqlite3_open))

(define-sqlite sqlite3_close
  (_fun _sqlite3_database
        -> _int))

;; -- Stmt --

(define (trim-and-copy-buffer buffer)
  (let* ([buffer (string->bytes/utf-8 (string-trim #:left? #f buffer))]
         [n (bytes-length buffer)]
         [rawcopy (malloc (add1 n) 'atomic-interior)])
    (memcpy rawcopy buffer n)
    (ptr-set! rawcopy _byte n 0)
    rawcopy))

(define (c-string-length p)
  (let loop ([i 0])
    (if (zero? (ptr-ref p _byte i))
        i
        (loop (add1 i)))))

(define (points-to-end? tail sql-buffer)
  (ptr-equal? tail
              (ptr-add sql-buffer (c-string-length sql-buffer))))

(define-sqlite sqlite3_prepare
  (_fun (db sql) ::
        (db : _sqlite3_database)
        (sql-buffer : _gcpointer = (trim-and-copy-buffer sql))
        ((c-string-length sql-buffer) : _int)
        (statement : (_ptr o _sqlite3_statement/null))
        (tail : (_ptr o _pointer)) ;; points into sql-buffer (atomic-interior)
        -> (result : _int)
        -> (values result statement (and tail
                                         (not (points-to-end? tail sql-buffer))))))

(define-sqlite sqlite3_prepare_v2
  (_fun (db sql) ::
        (db : _sqlite3_database)
        (sql-buffer : _gcpointer = (trim-and-copy-buffer sql))
        ((c-string-length sql-buffer) : _int)
        ;; bad prepare statements set statement to NULL, with no error reported
        (statement : (_ptr o _sqlite3_statement/null))
        (tail : (_ptr o _pointer)) ;; points into sql-buffer (atomic-interior)
        -> (result : _int)
        -> (values result statement (and tail
                                         (not (points-to-end? tail sql-buffer)))))
  #:fail (lambda () sqlite3_prepare))

(define-sqlite sqlite3_finalize
  (_fun _sqlite3_statement
        -> _int
        ;; sqlite3_finalize returns error code of last stmt execution,
        ;; not of finalization; so just ignore
        -> (void)))

(define-sqlite sqlite3_bind_parameter_count
  (_fun _sqlite3_statement
        -> _int))

(define-sqlite sqlite3_column_count
  (_fun _sqlite3_statement
        -> _int))
(define-sqlite sqlite3_column_name
  (_fun _sqlite3_statement _int
        -> _string))
(define-sqlite sqlite3_column_decltype
  (_fun _sqlite3_statement _int
        -> _string))

;; ----------------------------------------

(define-sqlite sqlite3_errcode
  (_fun _sqlite3_database -> _int))
(define-sqlite sqlite3_errmsg
  (_fun _sqlite3_database -> _string))

(define-sqlite sqlite3_extended_result_codes
  (_fun _sqlite3_database _bool -> _int)
  ;; Ok if it's unavailable:
  #:fail (lambda () (lambda (db on?) 0)))

;; ----------------------------------------

(define-sqlite sqlite3_bind_int
  (_fun _sqlite3_statement _int _int -> _int))
(define-sqlite sqlite3_bind_int64
  (_fun _sqlite3_statement _int _int64 -> _int))
(define-sqlite sqlite3_bind_double
  (_fun _sqlite3_statement _int _double -> _int))
(define-sqlite sqlite3_bind_text
  (_fun (stmt col the-string) ::
        (stmt : _sqlite3_statement)
        (col : _int)
        (string-ptr : _string = the-string)
        (string-len : _int = (string-utf-8-length the-string))
        (destructor : _intptr = SQLITE_TRANSIENT)
        -> _int))
(define-sqlite sqlite3_bind_blob
  (_fun (stmt col the-bytes) ::
        (stmt : _sqlite3_statement)
        (col : _int)
        (byte-ptr : _bytes = the-bytes)
        (byte-len : _int = (bytes-length the-bytes))
        (destructor : _intptr = SQLITE_TRANSIENT)
        -> _int))
(define-sqlite sqlite3_bind_null
  (_fun _sqlite3_statement _int -> _int))

(define-sqlite sqlite3_reset
  (_fun _sqlite3_statement -> _int))

(define-sqlite sqlite3_clear_bindings
  (_fun _sqlite3_statement -> _int)
  #:fail (lambda ()
           ;; Old versions of SQLite don't have sqlite3_clear_bindings().
           ;; With this fallback, some SQLite internal parameter
           ;; buffers won't get cleared at the end of statement
           ;; execution; they'll get cleared when the statement is
           ;; next executed or when the statement is closed instead.
           (lambda (stmt) 0)))

;; ----------------------------------------

(define-sqlite sqlite3_step
  (_fun _sqlite3_statement -> _int))

(define-sqlite sqlite3_column_type
  (_fun _sqlite3_statement _int -> _int))
(define-sqlite sqlite3_column_int
  (_fun _sqlite3_statement _int -> _int))
(define-sqlite sqlite3_column_int64
  (_fun _sqlite3_statement _int -> _int64))
(define-sqlite sqlite3_column_double
  (_fun _sqlite3_statement _int -> _double))
(define-sqlite sqlite3_column_text
  (_fun _sqlite3_statement _int -> _string))
(define-sqlite sqlite3_column_bytes
  (_fun _sqlite3_statement _int -> _int))
(define-sqlite sqlite3_column_blob
  (_fun (stmt : _sqlite3_statement)
        (col : _int)
        -> (blob : _pointer)
        -> (let* ([len (sqlite3_column_bytes stmt col)]
                  [bstr (make-bytes len)])
             (memcpy bstr blob len)
             bstr)))

;; ----------------------------------------

(define-sqlite sqlite3_get_autocommit
  (_fun _sqlite3_database
        -> _bool))

(define-sqlite sqlite3_sql
  (_fun _sqlite3_statement
        -> _string))

(define-sqlite sqlite3_changes
  (_fun _sqlite3_database
        -> _int))

(define-sqlite sqlite3_total_changes
  (_fun _sqlite3_database
        -> _int))

(define-sqlite sqlite3_last_insert_rowid
  (_fun _sqlite3_database
        -> _int64))

;; ----------------------------------------

(define SQLITE_DBCONFIG_ENABLE_LOAD_EXTENSION 1005) ;; int int*

(define-sqlite sqlite3_db_config
  (_fun _sqlite3_database _int _int (out : (_ptr o _int))
        -> (r : _int) -> r)) ;; FIXME: return out?

(define-sqlite sqlite3_enable_load_extension
  (_fun _sqlite3_database _int -> _int))

(define-sqlite sqlite3_load_extension
  ;; FIXME: handle error string?
  (_fun _sqlite3_database _path (_pointer = #f) (_pointer = #f)
        -> _int))

;; ----------------------------------------

(define-cpointer-type _sqlite3_context)
(define-cpointer-type _sqlite3_value)

(define-sqlite sqlite3_value_type (_fun _sqlite3_value -> _int))
(define-sqlite sqlite3_value_double (_fun _sqlite3_value -> _double))
(define-sqlite sqlite3_value_int64 (_fun _sqlite3_value -> _int64))
(define-sqlite sqlite3_value_bytes (_fun _sqlite3_value -> _int))
(define-sqlite sqlite3_value_blob (_fun _sqlite3_value -> _pointer))
(define-sqlite sqlite3_value_text (_fun _sqlite3_value -> _pointer))

(define (pointer->bytes p len)
  (define bstr (make-bytes len))
  (memcpy bstr p len)
  bstr)

(define _sqlite3_value*
  (make-ctype _sqlite3_value
              #f
              (lambda (v)
                (define type (sqlite3_value_type v))
                (cond [(= type SQLITE_INTEGER) (sqlite3_value_int64 v)]
                      [(= type SQLITE_FLOAT)   (sqlite3_value_double v)]
                      [(= type SQLITE_TEXT)
                       (bytes->string/utf-8 (pointer->bytes (sqlite3_value_text v)
                                                            (sqlite3_value_bytes v)))]
                      [(= type SQLITE_BLOB)
                       (pointer->bytes (sqlite3_value_blob v)
                                       (sqlite3_value_bytes v))]
                      [else (error '_sqlite3_value* "cannot convert: ~e (type = ~s)" v type)]))))

(define-sqlite sqlite3_create_function_v2
  (_fun _sqlite3_database
        _string/utf-8
        _int
        (_int = (+ SQLITE_UTF8 SQLITE_DETERMINISTIC))
        (_pointer = #f)
        (_fun _sqlite3_context _int _pointer -> _void)
        (_fpointer = #f)
        (_fpointer = #f)
        (_fpointer = #f)
        -> _int))

(define-sqlite sqlite3_create_aggregate
  (_fun _sqlite3_database
        _string/utf-8
        _int
        (_int = (+ SQLITE_UTF8 SQLITE_DETERMINISTIC))
        (_pointer = #f)
        (_fpointer = #f)
        (_fun _sqlite3_context _int _pointer -> _void)
        (_fun _sqlite3_context -> _void)
        (_fpointer = #f)
        -> _int)
  #:c-id sqlite3_create_function_v2)

(define-sqlite sqlite3_aggregate_context
  (_fun _sqlite3_context _int -> _pointer))

(define-sqlite sqlite3_result_null (_fun _sqlite3_context -> _void))
(define-sqlite sqlite3_result_int64 (_fun _sqlite3_context _int64 -> _void))
(define-sqlite sqlite3_result_double (_fun _sqlite3_context _double* -> _void))
(define-sqlite sqlite3_result_blob
  (_fun _sqlite3_context
        (buf : _bytes)
        (_int = (bytes-length buf))
        (_intptr = SQLITE_TRANSIENT)
        -> _void))
(define-sqlite sqlite3_result_text
  (_fun _sqlite3_context
        (buf : _string/utf-8)
        (_int = (string-utf-8-length buf))
        (_intptr = SQLITE_TRANSIENT)
        -> _void))
(define-sqlite sqlite3_result_error
  (_fun _sqlite3_context (s : _string/utf-8) (_int = (string-utf-8-length s)) -> _void))

(define ((wrap-fun who proc) ctx argc argp)
  (define args (get-args argc argp))
  (call/wrap who ctx (lambda () (sqlite3_result* ctx (apply proc args)))))

;; sqlite3 supports an "aggregate context" for storing aggregate
;; state, but it's hidden from Racket's GC. So instead we make a
;; closure with Racket-visible state and use sqlite's aggregate
;; context just to tell us whether we need to reset the Racket-level
;; state. The connection object is responsible for preventing the
;; closure from being prematurely collected.

(define ((wrap-agg-step who proc aggbox agginit) ctx argc argp)
  (define args (get-args argc argp))
  (define aggctx (sqlite3_aggregate_context ctx 1))
  (when (zero? (ptr-ref aggctx _byte))
    (set-box! aggbox agginit)
    (ptr-set! aggctx _byte 1))
  (set-box! aggbox (call/wrap who ctx (lambda () (apply proc (unbox aggbox) args))))
  (sqlite3_result* ctx 0))

(define ((wrap-agg-final who proc aggbox agginit) ctx)
  (define aggctx (sqlite3_aggregate_context ctx 1))
  (define r (call/wrap who ctx (lambda () (proc (unbox aggbox)))))
  (set-box! aggbox agginit)
  (sqlite3_result* ctx r))

(define (call/wrap who ctx proc)
  (with-handlers
    ([(lambda (e) #t)
      (lambda (e)
        (define err
          (format "[racket:~a] ~a"
                  who
                  (cond [(exn? e) (exn-message e)]
                        [else (format "caught non-exception\n  caught: ~e" e)])))
        (sqlite3_result_error ctx err))])
    (call-with-continuation-barrier proc)))

(define (get-args argc argp)
  (for/list ([i (in-range argc)])
    (ptr-ref argp _sqlite3_value* i)))

(define (sqlite3_result* ctx r)
  (cond [(fixnum? r) (sqlite3_result_int64 ctx r)] ;; FIXME: fixnum -> int64
        [(real? r) (sqlite3_result_double ctx r)]
        [(string? r) (sqlite3_result_text ctx r)]
        [(bytes? r) (sqlite3_result_blob ctx r)]
        [else (sqlite3_result_error ctx (format "bad result: ~e" r))]))
