#lang racket/base
(require racket/class
         racket/list
         racket/math
         ffi/unsafe
         ffi/unsafe/atomic
         unstable/error
         db/private/generic/interfaces
         db/private/generic/common
         db/private/generic/prepared
         db/private/generic/sql-data
         db/private/generic/sql-convert
         "ffi.rkt"
         "ffi-constants.rkt"
         "dbsystem.rkt")
(provide connection%
         handle-status*
         dbsystem)

;; == Connection

;; ODBC connections do not use statement-cache%
;;  - safety depends on sql dialect
;;  - transaction interactions more complicated

(define connection%
  (class* transactions% (connection<%>)
    (init-private db
                  env
                  notice-handler
                  char-mode)
    (init strict-parameter-types?)
    (super-new)

    (define statement-table (make-hasheq))

    (define use-describe-param?
      (and strict-parameter-types?
           (let-values ([(status supported?) (SQLGetFunctions db SQL_API_SQLDESCRIBEPARAM)])
             (handle-status 'odbc-connect status db)
             supported?)))

    (define dbms
      (let-values ([(status result) (SQLGetInfo-string db SQL_DBMS_NAME)])
        (handle-status 'odbc-connect status db)
        result))

    (inherit call-with-lock
             call-with-lock*
             add-delayed-call!
             get-tx-status
             set-tx-status!
             check-valid-tx-status
             check-statement/tx)

    (define/public (get-db fsym)
      (unless db
        (error/not-connected fsym))
      db)

    (define/public (get-dbsystem) dbsystem)
    (define/override (connected?) (and db #t))

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
        (let ([result-dvecs (send pst get-result-dvecs)])
          (for ([dvec (in-list result-dvecs)])
            (let ([typeid (field-dvec->typeid dvec)])
              (unless (supported-typeid? typeid)
                (error/unsupported-type fsym typeid)))))
        (query1:inner fsym pst params cursor?)))

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

    (define/private (query1:inner fsym pst params cursor?)
      (let* ([db (get-db fsym)]
             [stmt (send pst get-handle)])
        (let* ([param-bufs
                ;; Need to keep references to all bufs until after SQLExecute.
                (for/list ([i (in-naturals 1)]
                           [param (in-list params)]
                           [param-typeid (in-list (send pst get-param-typeids))])
                  (load-param fsym db stmt i param param-typeid))])
          (handle-status fsym (SQLExecute stmt) stmt)
          (strong-void param-bufs))
        (define result-dvecs (send pst get-result-dvecs))
        (set-result-descriptors stmt result-dvecs)
        (define rows
          (and (not cursor?)
               (pair? result-dvecs)
               (fetch* fsym stmt (map field-dvec->typeid result-dvecs) #f +inf.0)))
        (unless cursor? (send pst after-exec #f))
        (cond [(and (pair? result-dvecs) (not cursor?))
               (rows-result (map field-dvec->field-info result-dvecs) rows)]
              [(and (pair? result-dvecs) cursor?)
               (cursor-result (map field-dvec->field-info result-dvecs)
                              pst
                              (list (map field-dvec->typeid result-dvecs)
                                    (box #f)))]
              [else (simple-result '())])))

    (define/public (fetch/cursor fsym cursor fetch-size)
      (let ([pst (cursor-result-pst cursor)]
            [extra (cursor-result-extra cursor)])
        (send pst check-owner fsym this pst)
        (call-with-lock fsym
          (lambda ()
            (let ([typeids (car extra)]
                  [end-box (cadr extra)])
              (cond [(unbox end-box) #f]
                    [else
                     (begin0 (fetch* fsym (send pst get-handle) typeids end-box fetch-size)
                       (when (unbox end-box)
                         (send pst after-exec #f)))]))))))

    (define/private (load-param fsym db stmt i param typeid)
      ;; NOTE: param buffers must not move between bind and execute
      ;; So use buffer utils from ffi.rkt (copy-buffer, etc)
      (define (bind ctype sqltype buf [prec 0] [scale 0])
        (let* ([lenbuf
                (int->buffer (if buf (bytes-length buf) SQL_NULL_DATA))]
               [status
                (SQLBindParameter stmt i SQL_PARAM_INPUT ctype sqltype prec scale buf lenbuf)])
          (handle-status fsym status stmt)
          (if buf (cons buf lenbuf) lenbuf)))
      ;; If the typeid is UNKNOWN, then choose appropriate type based on data,
      ;; but respect typeid if known.
      (define unknown-type? (= typeid SQL_UNKNOWN_TYPE))
      (cond [(string? param)
             (case char-mode
               ((wchar)
                (bind SQL_C_WCHAR (if unknown-type? SQL_WVARCHAR typeid)
                      (case WCHAR-SIZE
                        ((2) (cpstr2 param))
                        ((4) (cpstr4 param)))))
               ((utf-8)
                (bind SQL_C_CHAR (if unknown-type? SQL_VARCHAR typeid)
                      (copy-buffer (string->bytes/utf-8 param))))
               ((latin-1)
                (bind SQL_C_CHAR (if unknown-type? SQL_VARCHAR typeid)
                      (copy-buffer (string->bytes/latin-1 param (char->integer #\?))))))]
            [(bytes? param)
             (bind SQL_C_BINARY (if unknown-type? SQL_BINARY typeid)
                   (copy-buffer param))]
            [(pair? param) ;; Represents numeric/decimal decomposed as scaled integer
             (let* ([ma (car param)]
                    [ex (cdr param)]
                    ;; ODBC docs claim max precision is 15 ...
                    [prec-byte (if (zero? ma) 1 (+ 1 (order-of-magnitude (abs ma))))]
                    [sign-byte (if (negative? ma) 0 1)] ;; FIXME: negative is 2 in ODBC 3.5 ???
                    [digits-bytess
                     ;; 16 bytes of unsigned little-endian data (4 chunks of 4 bytes)
                     (let loop ([i 0] [ma (abs ma)])
                       (if (< i 4)
                           (let-values ([(q r) (quotient/remainder ma (expt 2 32))])
                             (cons (integer->integer-bytes r 4 #f #f)
                                   (loop (add1 i) q)))
                           null))]
                    [numeric-bytes
                     (apply bytes-append (bytes prec-byte ex sign-byte) digits-bytess)]
                    [numeric-buffer (copy-buffer numeric-bytes)])
               ;; Example: http://support.microsoft.com/kb/181254
               ;; and: http://msdn.microsoft.com/en-us/library/ms712567%28v=vs.85%29.aspx
               ;; Call bind first.
               (bind SQL_C_NUMERIC typeid numeric-buffer prec-byte ex)
               ;; Then set descriptor attributes.
               (set-numeric-descriptors (SQLGetStmtAttr/HDesc stmt SQL_ATTR_APP_PARAM_DESC)
                                        i prec-byte ex numeric-buffer))]
            [(real? param)
             (cond [(or (= typeid SQL_NUMERIC) (= typeid SQL_DECIMAL))
                    (bind SQL_C_CHAR typeid
                          (copy-buffer (marshal-decimal fsym i param)))]
                   [(or (and unknown-type? (int32? param))
                        (= typeid SQL_INTEGER)
                        (= typeid SQL_SMALLINT)
                        (= typeid SQL_BIGINT)
                        (= typeid SQL_TINYINT))
                    ;; Oracle errors without diagnostic record (!!) on BIGINT param
                    ;; -> http://stackoverflow.com/questions/338609
                    ;; FIXME: find a better solution, eg check driver for BIGINT support (?)
                    (if (= typeid SQL_BIGINT)
                        (bind SQL_C_SBIGINT SQL_BIGINT
                              (copy-buffer (integer->integer-bytes param 8 #t)))
                        (bind SQL_C_LONG (if unknown-type? SQL_INTEGER typeid)
                              (copy-buffer (integer->integer-bytes param 4 #t))))]
                   [else
                    (bind SQL_C_DOUBLE (if unknown-type? SQL_DOUBLE typeid)
                          (copy-buffer
                           (real->floating-point-bytes (exact->inexact param) 8)))])]
            [(boolean? param)
             (bind SQL_C_LONG SQL_BIT
                   (copy-buffer (int->buffer (if param 1 0))))]
            [(sql-date? param)
             (bind SQL_C_TYPE_DATE SQL_TYPE_DATE
                   (copy-buffer
                    (let* ([x param]
                           [y (sql-date-year x)]
                           [m (sql-date-month x)]
                           [d (sql-date-day x)])
                      (bytes-append (integer->integer-bytes y 2 #t)
                                    (integer->integer-bytes m 2 #f)
                                    (integer->integer-bytes d 2 #f)))))]
            [(sql-time? param)
             (bind SQL_C_TYPE_TIME SQL_TYPE_TIME
                   (copy-buffer
                    (let* ([x param]
                           [h (sql-time-hour x)]
                           [m (sql-time-minute x)]
                           [s (sql-time-second x)])
                      (bytes-append (integer->integer-bytes h 2 #f)
                                    (integer->integer-bytes m 2 #f)
                                    (integer->integer-bytes s 2 #f)))))]
            [(sql-timestamp? param)
             (bind SQL_C_TYPE_TIMESTAMP
                   (if unknown-type? SQL_TYPE_TIMESTAMP typeid)
                   (copy-buffer
                    (let ([x param])
                      (bytes-append
                       (integer->integer-bytes (sql-timestamp-year x) 2 #f)
                       (integer->integer-bytes (sql-timestamp-month x) 2 #f)
                       (integer->integer-bytes (sql-timestamp-day x) 2 #f)
                       (integer->integer-bytes (sql-timestamp-hour x) 2 #f)
                       (integer->integer-bytes (sql-timestamp-minute x) 2 #f)
                       (integer->integer-bytes (sql-timestamp-second x) 2 #f)
                       (integer->integer-bytes (sql-timestamp-nanosecond x) 4 #f)))))]
            [(sql-null? param)
             (bind SQL_C_CHAR SQL_VARCHAR #f)]
            [else (error/internal* fsym "cannot convert given value to SQL type"
                                   '("given" value) param
                                   "typeid" typeid)]))

    (define/private (set-result-descriptors stmt dvecs)
      (for ([i (in-naturals 1)]
            [dvec (in-list dvecs)])
        (define typeid (field-dvec->typeid dvec))
        (cond [(or (= typeid SQL_DECIMAL)
                   (= typeid SQL_NUMERIC))
               (define hdesc (SQLGetStmtAttr/HDesc stmt SQL_ATTR_APP_ROW_DESC))
               (define size (field-dvec->size dvec))
               (define digits (field-dvec->digits dvec))
               (set-numeric-descriptors hdesc i size digits #f)]
              [else (void)])))

    (define/private (set-numeric-descriptors hdesc i prec ex buf)
      (SQLSetDescField/Int hdesc i SQL_DESC_TYPE SQL_C_NUMERIC)
      (SQLSetDescField/Int hdesc i SQL_DESC_PRECISION prec)
      (SQLSetDescField/Int hdesc i SQL_DESC_SCALE ex)
      (when buf (SQLSetDescField/Ptr hdesc i SQL_DESC_DATA_PTR buf (bytes-length buf))))

    (define/private (fetch* fsym stmt result-typeids end-box limit)
      ;; scratchbuf: create a single buffer here to try to reduce garbage
      ;; Don't make too big; otherwise bad for queries with only small data.
      ;; Doesn't need to be large, since get-varbuf already smart for long data.
      ;; MUST be at least as large as any int/float type (see get-num)
      ;; SHOULD be at least as large as any structures (see uses of get-int-list)
      (let ([scratchbuf (make-bytes 50)]) 
        (let loop ([fetched 0])
          (cond [(< fetched limit)
                 (let ([c (fetch fsym stmt result-typeids scratchbuf)])
                   (cond [c
                          (cons c (loop (add1 fetched)))]
                         [else
                          (when end-box (set-box! end-box #t))
                          (handle-status fsym (SQLFreeStmt stmt SQL_CLOSE) stmt)
                          (handle-status fsym (SQLFreeStmt stmt SQL_RESET_PARAMS) stmt)
                          null]))]
                [else null]))))

    (define/private (fetch fsym stmt result-typeids scratchbuf)
      (let ([s (SQLFetch stmt)])
        (cond [(= s SQL_NO_DATA) #f]
              [(= s SQL_SUCCESS)
               (let* ([column-count (length result-typeids)]
                      [vec (make-vector column-count)])
                 (for ([i (in-range column-count)]
                       [typeid (in-list result-typeids)])
                   (vector-set! vec i (get-column fsym stmt (add1 i) typeid scratchbuf)))
                 vec)]
              [else (handle-status fsym s stmt)])))

    (define/private (get-column fsym stmt i typeid scratchbuf)
      (define-syntax-rule (get-num size ctype convert convert-arg ...)
        (let-values ([(status ind) (SQLGetData stmt i ctype scratchbuf 0)])
          (handle-status fsym status stmt)
          (cond [(= ind SQL_NULL_DATA) sql-null]
                [else (convert scratchbuf convert-arg ... 0 size)])))
      (define (get-int size ctype)
        (get-num size ctype integer-bytes->integer     #t (system-big-endian?)))
      (define (get-real ctype)
        (get-num 8    ctype floating-point-bytes->real (system-big-endian?)))
      (define (get-int-list sizes ctype)
        (let* ([buflen (apply + sizes)]
               [buf (if (<= buflen (bytes-length scratchbuf)) scratchbuf (make-bytes buflen))])
          (let-values ([(status ind) (SQLGetData stmt i ctype buf 0)])
            (handle-status fsym status stmt)
            (cond [(= ind SQL_NULL_DATA) sql-null]
                  [else (let ([in (open-input-bytes buf)])
                          (for/list ([size (in-list sizes)])
                            (case size
                              ((1) (read-byte in))
                              ((2) (integer-bytes->integer (read-bytes 2 in) #f))
                              ((4) (integer-bytes->integer (read-bytes 4 in) #f))
                              (else (error/internal
                                     'get-int-list "bad size: ~e" size)))))]))))

      (define (get-varbuf ctype ntlen convert)
        ;; ntlen is null-terminator length (1 for char data, 0 for binary, ??? for wchar)

        ;; null-terminator, there are 3 modes: binary, wchar=2, wchar=4
        ;; - binary: all done in racket, no worries
        ;; - wchar=4: passed to make_sized_char_string, so must explicitly leave ntlen \0 bytes
        ;; - wchar=2: passed to utf16_to_ucs4 (which must add \0 space, see ffi.rkt)
        ;; So for simplicity, add ntlen \0 bytes to buf (but do *not* add to data len)

        ;; ODBC docs say len-or-ind is character count for char data, but wrong:
        ;; always a byte count.

        ;; It would be nice if we could call w/ empty buffer, get total length, then
        ;; call again with appropriate buffer. But can't use NULL (works on unixodbc, but
        ;; ODBC spec says illegal and Win32 ODBC rejects). Seems unsafe to use 0-length
        ;; buffer (spec is unclear, DB2 docs say len>0...???).

        ;; loop : bytes nat (listof bytes) -> any
        ;; start is next place to write, but data starts at 0
        ;; rchunks is reversed list of previous chunks (for data longer than scratchbuf)
        ;; Small data done in one iteration; most long data done in two. Only long data
        ;; without known size (???) should take more than two iterations.
        (define (loop buf start rchunks)
          (let-values ([(status len-or-ind) (SQLGetData stmt i ctype buf start)])
            (handle-status fsym status stmt #:ignore-ok/info? #t)
            (cond [(= len-or-ind SQL_NULL_DATA) sql-null]
                  [(= len-or-ind SQL_NO_TOTAL)
                   ;; didn't fit in buf, and we have no idea how much more there is
                   ;; start = 0
                   (let* ([data-end (- (bytes-length buf) ntlen)])
                     (loop buf 0 (cons (subbytes buf 0 data-end) rchunks)))]
                  [else
                   (let ([len (+ start len-or-ind)])
                     (cond [(<= 0 len (- (bytes-length buf) ntlen))
                            ;; fit in buf
                            (cond [(pair? rchunks)
                                   ;; add ntlen bytes for null-terminator...
                                   (let* ([chunk (subbytes buf 0 (+ len ntlen))]
                                          [chunks (append (reverse rchunks) (list chunk))]
                                          [complete (apply bytes-append chunks)])
                                     ;; ... but compensate so len is correct
                                     (convert complete (- (bytes-length complete) ntlen) #t))]
                                  [else
                                   ;; buf already null-terminated, len correct
                                   (convert buf len #f)])]
                           [else
                            ;; didn't fit in buf, but we know how much more there is
                            (let* ([len-got (- (bytes-length buf) ntlen)]
                                   [newbuf (make-bytes (+ len ntlen))])
                              (bytes-copy! newbuf 0 buf start len-got)
                              (loop newbuf len-got rchunks))]))])))
        (loop scratchbuf 0 null))

      (define (get-string/latin-1)
        (get-varbuf SQL_C_CHAR 1
                    (lambda (buf len _fresh?)
                      (bytes->string/latin-1 buf #f 0 len))))
      (define (get-string/utf-8)
        (get-varbuf SQL_C_CHAR 1
                    (lambda (buf len _fresh?)
                      (bytes->string/utf-8 buf #f 0 len))))
      (define (get-string)
        (case char-mode
          ((wchar)
           (get-varbuf SQL_C_WCHAR WCHAR-SIZE (case WCHAR-SIZE ((2) mkstr2) ((4) mkstr4))))
          ((utf-8)
           (get-string/utf-8))
          ((latin-1)
           (get-string/latin-1))))
      (define (get-bytes)
        (get-varbuf SQL_C_BINARY 0
                    (lambda (buf len fresh?)
                      ;; avoid copying long data twice:
                      (if (and fresh? (= len (bytes-length buf)))
                          buf
                          (subbytes buf 0 len)))))

      (cond [(or (= typeid SQL_CHAR)
                 (= typeid SQL_VARCHAR)
                 (= typeid SQL_LONGVARCHAR)
                 (= typeid SQL_WCHAR)
                 (= typeid SQL_WVARCHAR)
                 (= typeid SQL_WLONGVARCHAR))
             (get-string)]
            [(or (= typeid SQL_DECIMAL)
                 (= typeid SQL_NUMERIC))
             (let ([fields (get-int-list '(1 1 1 4 4 4 4) SQL_ARD_TYPE)])
               (cond [(list? fields)
                      (let* ([precision (first fields)]
                             [scale (second fields)]
                             [sign (case (third fields) ((0) -1) ((1) 1))]
                             [ma (let loop ([lst (cdddr fields)])
                                   (if (pair? lst)
                                       (+ (* (loop (cdr lst)) (expt 2 32))
                                          (car lst))
                                       0))])
                        ;; (eprintf "numeric: ~s\n" fields)
                        (* sign ma (expt 10 (- scale))))]
                     [(sql-null? fields) sql-null]))]
            [(or (= typeid SQL_SMALLINT)
                 (= typeid SQL_INTEGER)
                 (= typeid SQL_TINYINT))
             (get-int 4 SQL_C_LONG)]
            [(or (= typeid SQL_BIGINT))
             (get-int 8 SQL_C_SBIGINT)]
            [(or (= typeid SQL_REAL)
                 (= typeid SQL_FLOAT)
                 (= typeid SQL_DOUBLE))
             (get-real SQL_C_DOUBLE)]
            [(or (= typeid SQL_BIT))
             (case (get-int 4 SQL_C_LONG)
               ((0) #f)
               ((1) #t)
               (else 'get-column "internal error: SQL_BIT"))]
            [(or (= typeid SQL_BINARY)
                 (= typeid SQL_VARBINARY))
             (get-bytes)]
            [(= typeid SQL_TYPE_DATE)
             (let ([fields (get-int-list '(2 2 2) SQL_C_TYPE_DATE)])
               (cond [(list? fields) (apply sql-date fields)]
                     [(sql-null? fields) sql-null]))]
            [(= typeid SQL_TYPE_TIME)
             (let ([fields (get-int-list '(2 2 2) SQL_C_TYPE_TIME)])
               (cond [(list? fields) (apply sql-time (append fields (list 0 #f)))]
                     [(sql-null? fields) sql-null]))]
            [(= typeid SQL_TYPE_TIMESTAMP)
             (let ([fields (get-int-list '(2 2 2 2 2 2 4) SQL_C_TYPE_TIMESTAMP)])
               (cond [(list? fields) (apply sql-timestamp (append fields (list #f)))]
                     [(sql-null? fields) sql-null]))]
            [else (get-string)]))

    (define/public (prepare fsym stmt close-on-exec?)
      (call-with-lock fsym
        (lambda ()
          (check-valid-tx-status fsym)
          (prepare1 fsym stmt close-on-exec?))))

    (define/private (prepare1 fsym sql close-on-exec?)
      ;; no time between prepare and table entry
      (let* ([stmt
              (let*-values ([(db) (get-db fsym)]
                            [(status stmt) (SQLAllocHandle SQL_HANDLE_STMT db)])
                (handle-status fsym status db)
                (with-handlers ([(lambda (e) #t)
                                 (lambda (e)
                                   (SQLFreeHandle SQL_HANDLE_STMT stmt)
                                   (raise e))])
                  (let ([status (SQLPrepare stmt sql)])
                    (handle-status fsym status stmt)
                    stmt)))]
             [param-typeids (describe-params fsym stmt)]
             [result-dvecs (describe-result-columns fsym stmt)])
        (let ([pst (new prepared-statement%
                        (handle stmt)
                        (close-on-exec? close-on-exec?)
                        (param-typeids param-typeids)
                        (result-dvecs result-dvecs)
                        (stmt sql)
                        (stmt-type (classify-odbc-sql sql))
                        (owner this))])
          (hash-set! statement-table pst #t)
          pst)))

    (define/private (describe-params fsym stmt)
      (let-values ([(status param-count) (SQLNumParams stmt)])
        (handle-status fsym status stmt)
        (for/list ([i (in-range 1 (add1 param-count))])
          (cond [use-describe-param?
                 (let-values ([(status type size digits nullable)
                               (SQLDescribeParam stmt i)])
                   (handle-status fsym status stmt)
                   type)]
                [else SQL_UNKNOWN_TYPE]))))

    (define/private (describe-result-columns fsym stmt)
      (let-values ([(status result-count) (SQLNumResultCols stmt)]
                   [(scratchbuf) (make-bytes 200)])
        (handle-status fsym status stmt)
        (for/list ([i (in-range 1 (add1 result-count))])
          (let-values ([(status name type size digits nullable)
                        (SQLDescribeCol stmt i scratchbuf)])
            (handle-status fsym status stmt)
            (vector name type size digits)))))

    (define/override (disconnect* _politely?)
      (super disconnect* _politely?)
      (start-atomic)
      (let ([db* db]
            [env* env])
        (set! db #f)
        (set! env #f)
        (end-atomic)
        (when db*
          (let ([statements (hash-map statement-table (lambda (k v) k))])
            (for ([pst (in-list statements)])
              (free-statement* 'disconnect pst))
            (handle-status 'disconnect (SQLDisconnect db*) db*)
            (handle-status 'disconnect (SQLFreeHandle SQL_HANDLE_DBC db*))
            (handle-status 'disconnect (SQLFreeHandle SQL_HANDLE_ENV env*))
            (void)))))

    (define/public (get-base) this)

    (define/public (free-statement pst need-lock?)
      (define (go) (free-statement* 'free-statement pst))
      (if need-lock? 
          (call-with-lock* 'free-statement go go #f)
          (go)))

    (define/private (free-statement* fsym pst)
      (start-atomic)
      (let ([stmt (send pst get-handle)])
        (send pst set-handle #f)
        (end-atomic)
        (when stmt
          (handle-status 'free-statement (SQLFreeStmt stmt SQL_CLOSE) stmt)
          (handle-status 'free-statement (SQLFreeHandle SQL_HANDLE_STMT stmt) stmt)
          (hash-remove! statement-table pst)
          (void))))

    ;; Transactions

    (define/override (start-transaction* fsym isolation option)
      (when (eq? isolation 'nested)
        (error* fsym "already in transaction"
                #:continued "nested transactions not supported for ODBC connections"))
      (when option
        ;; No options supported
        (raise-argument-error fsym "#f" option))
      (let* ([db (get-db fsym)]
             [ok-levels
              (let-values ([(status value)
                            (SQLGetInfo db SQL_TXN_ISOLATION_OPTION)])
                (begin0 value (handle-status fsym status db)))]
             [default-level
               (let-values ([(status value)
                             (SQLGetInfo db SQL_DEFAULT_TXN_ISOLATION)])
                 (begin0 value (handle-status fsym status db)))]
             [requested-level
              (case isolation
                ((serializable) SQL_TXN_SERIALIZABLE)
                ((repeatable-read) SQL_TXN_REPEATABLE_READ)
                ((read-committed) SQL_TXN_READ_COMMITTED)
                ((read-uncommitted) SQL_TXN_READ_UNCOMMITTED)
                (else
                 ;; MySQL ODBC returns 0 for default level, seems no good.
                 ;; So if 0, use serializable.
                 (if (zero? default-level) SQL_TXN_SERIALIZABLE default-level)))])
        (when (zero? (bitwise-and requested-level ok-levels))
          (error* fsym "requested isolation level is not available"
                  '("isolation level" value) isolation))
        (let ([status (SQLSetConnectAttr db SQL_ATTR_TXN_ISOLATION requested-level)])
          (handle-status fsym status db)))
      (let ([status (SQLSetConnectAttr db SQL_ATTR_AUTOCOMMIT SQL_AUTOCOMMIT_OFF)])
        (handle-status fsym status db)
        (set-tx-status! fsym #t)
        (void)))

    (define/override (end-transaction* fsym mode _savepoint)
      ;; _savepoint = #f, because nested transactions not supported on ODBC
      (let ([db (get-db fsym)]
            [completion-type
             (case mode
               ((commit) SQL_COMMIT)
               ((rollback) SQL_ROLLBACK))])
        (handle-status fsym (SQLEndTran db completion-type) db)
        (let ([status (SQLSetConnectAttr db SQL_ATTR_AUTOCOMMIT SQL_AUTOCOMMIT_ON)])
          (handle-status fsym status db)
          ;; commit/rollback can fail; don't change status until possible error handled
          (set-tx-status! fsym #f)
          (void))))

    ;; GetTables

    (define/public (list-tables fsym schema)
      (define (no-search)
        (error fsym "schema search path cannot be determined for this DBMS"))
      (let ([stmt
             (cond
              [(regexp-match? #rx"^DB2" dbms)
               (let* ([schema-cond
                       (case schema
                         ((search-or-current current)
                          "tabschema = CURRENT_SCHEMA")
                         ((search)
                          (no-search)))]
                      [type-cond
                       ;; FIXME: what table types to include? see docs for SYSCAT.TABLES
                       "(type = 'T' OR type = 'V')"])
                 (string-append "SELECT tabname FROM syscat.tables "
                                "WHERE " type-cond " AND " schema-cond))]
              [(equal? dbms "Oracle")
               (let* ([schema-cond
                       (case schema
                         ((search-or-current current)
                          "owner = sys_context('userenv', 'current_schema')")
                         ((search)
                          (no-search)))])
                 (string-append "SELECT table_name AS name FROM sys.all_tables "
                                "WHERE " schema-cond
                                "UNION "
                                "SELECT view_name AS name FROM sys.all_views "
                                "WHERE " schema-cond))]
              [else
               (error fsym "not supported for this DBMS")])])
        (let* ([result (query fsym stmt #f)]
               [rows (rows-result-rows result)])
          (for/list ([row (in-list rows)])
            (vector-ref row 0)))))

    #|
    (define/public (get-tables fsym catalog schema table)
      (define-values (dvecs rows)
        (call-with-lock fsym
          (lambda ()
            (let* ([db (get-db fsym)]
                   [stmt (let-values ([(status stmt) (SQLAllocHandle SQL_HANDLE_STMT db)])
                           (handle-status fsym status db)
                           stmt)]
                   [_ (handle-status fsym (SQLTables stmt catalog schema table))]
                   [result-dvecs (describe-result-columns fsym stmt)]
                   [rows (fetch* fsym stmt (map field-dvec->typeid result-dvecs))])
              (handle-status fsym (SQLFreeStmt stmt SQL_CLOSE) stmt)
              (handle-status fsym (SQLFreeHandle SQL_HANDLE_STMT stmt) stmt)
              (values result-dvecs rows)))))
      ;; Layout is: #(catalog schema table table-type remark)
      (rows-result (map field-dvec->field-info dvecs)
                   rows))
    |#

    ;; Handler

    (define/private (handle-status who s [handle #f]
                                   #:ignore-ok/info? [ignore-ok/info? #f])
      (define (handle-error e)
        ;; On error, driver may rollback whole transaction, last statement, etc.
        ;; Options:
        ;;   1) if transaction was rolled back, set autocommit=true
        ;;   2) automatically rollback on error
        ;;   3) create flag: "transaction had error, please call rollback" (like pg)
        ;; Option 1 would be nice, but as far as I can tell, there's
        ;; no way to find out if the transaction was rolled back. And
        ;; it would be very bad to leave autocommit=false, because
        ;; that would be interpreted as "still in same transaction".
        ;; Go with (3) for now, maybe support (2) as option later.
        ;; FIXME: I worry about multi-statements like "<cause error>; commit"
        ;; if the driver does one-statement rollback.
        (let ([db db])
          (when db
            (when (get-tx-status)
              (set-tx-status! who 'invalid))))
        (raise e))
      ;; Be careful: shouldn't do rollback before we call handle-status*
      ;; just in case rollback destroys statement with diagnostic records.
      (with-handlers ([exn:fail? handle-error])
        (handle-status* who s handle
                        #:ignore-ok/info? ignore-ok/info?
                        #:on-notice (lambda (sqlstate msg)
                                      (add-delayed-call!
                                       (lambda () (notice-handler sqlstate msg)))))))

    (register-finalizer this
                        (lambda (obj)
                          ;; Keep a reference to the class to keep all FFI callout objects
                          ;; (eg, SQLDisconnect) used by its methods from being finalized.
                          (let ([dont-gc this%])
                            (send obj disconnect)
                            ;; Dummy result to prevent reference from being optimized away
                            dont-gc)))))

;; ----------------------------------------

(define (handle-status* who s [handle #f]
                        #:ignore-ok/info? [ignore-ok/info? #f]
                        #:on-notice [on-notice void])
  (cond [(= s SQL_SUCCESS_WITH_INFO)
         (when (and handle (not ignore-ok/info?))
           (diag-info who handle 'notice on-notice))
         s]
        [(= s SQL_ERROR)
         (when handle (diag-info who handle 'error #f))
         (error who "unknown error (no diagnostic returned)")]
        [else s]))

(define (diag-info who handle mode on-notice)
  (let ([handle-type
         (cond [(sqlhenv? handle) SQL_HANDLE_ENV]
               [(sqlhdbc? handle) SQL_HANDLE_DBC]
               [(sqlhstmt? handle) SQL_HANDLE_STMT]
               [else
                (error/internal* 'diag-info "unknown handle type" '("handle" value) handle)])])
    (let-values ([(status sqlstate native-errcode message)
                  (SQLGetDiagRec handle-type handle 1)])
      (case mode
        ((error)
         (raise-sql-error who sqlstate message
                          `((code . ,sqlstate)
                            (message . ,message)
                            (native-errcode . ,native-errcode))))
        ((notice)
         (on-notice sqlstate message))))))

;; ========================================

(define (marshal-decimal f n)
  (cond [(not (real? n))
         (error/no-convert f #f "numeric" n)]
        [(eqv? n +nan.0)
         "NaN"]
        [(or (eqv? n +inf.0) (eqv? n -inf.0))
         (error/no-convert f #f "numeric" n)]
        [(or (integer? n) (inexact? n))
         (number->string n)]
        [(exact? n)
         ;; Bleah.
         (or (exact->decimal-string n)
             (number->string (exact->inexact n)))]))

#|
Historical note: I tried using ODBC async execution to avoid blocking
all Racket threads for a long time.

1) The postgresql, mysql, and oracle drivers don't even support async
execution. Only DB2 (and probably SQL Server, but I didn't try it).

2) Tests using the DB2 driver gave baffling HY010 (function sequence
error). My best theory so far is that DB2 (or maybe unixodbc) requires
poll call arguments to be identical to original call arguments, which
means that I would have to replace all uses of (_ptr o X) with
something stable across invocations.

All in all, not worth it, especially given #:use-place solution.
|#
