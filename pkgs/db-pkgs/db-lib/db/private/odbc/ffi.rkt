#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         "ffi-constants.rkt")
(provide (all-from-out "ffi-constants.rkt"))
(provide (protect-out (all-defined-out)))

(define-cpointer-type _sqlhandle)

(define-cpointer-type _sqlhenv)
(define-cpointer-type _sqlhdbc)
(define-cpointer-type _sqlhstmt)
(define-cpointer-type _sqlhdesc)

(define _sqllen _long)
(define _sqlulen _ulong)

(define _sqlsmallint _sshort)
(define _sqlusmallint _ushort)
(define _sqlinteger _sint)
(define _sqluinteger _uint)
(define _sqlreturn _sqlsmallint)

(define-ffi-definer define-mz #f)

(define-mz scheme_utf16_to_ucs4
  (_fun (src srcstart srcend) ::
        (src : _bytes)
        (srcstart : _intptr)
        (srcend : _intptr)
        (#f : _pointer)   ;; No buffer so it'll allocate for us.
        (0 : _intptr)
        (clen : (_ptr o _intptr))
        (1 : _intptr)
        -> (out : _gcpointer)
        -> (begin (ptr-set! out _int32 clen 0)
                  (values out clen))))

(define-mz scheme_ucs4_to_utf16
  (_fun (src srcstart srcend) ::
        (src : _string/ucs-4)
        (srcstart : _intptr)
        (srcend : _intptr)
        (#f : _pointer)   ;; No buffer so it'll allocate for us.
        (0 : _intptr)
        (clen : (_ptr o _intptr))
        (1 : _intptr)
        -> (out : _gcpointer)
        -> (begin (ptr-set! out _int16 clen 0)
                  (values out clen))))

(define-mz scheme_make_sized_char_string
  (_fun (chars clen copy?) ::
        (chars : _gcpointer)
        (clen  : _intptr)
        (copy? : _bool)
        -> _racket))

(define scheme_make_sized_byte_string/string
  (get-ffi-obj 'scheme_make_sized_byte_string #f
               (_fun (buf len) ::
                     (buf : _string/ucs-4)
                     (len  : _intptr)
                     (#t : _bool)
                     -> _racket)))

;; For dealing with param buffers, which must not be moved by GC

(define (copy-buffer buffer)
  (let* ([buffer (if (string? buffer) (string->bytes/utf-8 buffer) buffer)]
         [n (bytes-length buffer)]
         [rawcopy (malloc (add1 n) 'atomic-interior)]
         [copy (make-sized-byte-string rawcopy n)])
    (memcpy copy buffer n)
    (ptr-set! rawcopy _byte n 0)
    copy))

(define (int->buffer n)
  (let ([copy (make-sized-byte-string (malloc 4 'atomic-interior) 4)])
    (integer->integer-bytes n 4 #t (system-big-endian?) copy 0)
    copy))

(define (cpstr2 str)
  (let-values ([(shorts slen) (scheme_ucs4_to_utf16 str 0 (string-length str))])
    (let* ([n (* slen 2)]
           [rawcopy (malloc (add1 n) 'atomic-interior)]
           [copy (make-sized-byte-string rawcopy n)])
      (memcpy copy shorts n)
      (ptr-set! rawcopy _byte n 0)
      copy)))

(define (cpstr4 str)
  (copy-buffer (scheme_make_sized_byte_string/string str (* (string-length str) 4))))

(define (mkstr2 buf len fresh?)
  (let-values ([(chars clen) (scheme_utf16_to_ucs4 buf 0 (quotient len 2))])
    (scheme_make_sized_char_string chars clen #f)))

(define (mkstr4 buf len fresh?)
  (scheme_make_sized_char_string buf (quotient len 4) (not fresh?)))

;; ========================================

;; Used in connection.rkt; silly hack to keep optimizer from eliminating ref to
;; things that shouldn't be GC'd. Depends on no cross-module inlining.
(define (strong-void x) (void))

;; ========================================

#|
Docs at http://msdn.microsoft.com/en-us/library/ms712628%28v=VS.85%29.aspx
|#

(define-values (odbc-lib WCHAR-SIZE)
  (case (system-type)
    ((windows)
     ;; Windows ODBC defines wchar_t (thus WCHAR, thus SQLWCHAR) as 16-bit
     (values (ffi-lib "odbc32.dll")
             2))
    ((macosx)
     ;; Mac OS X uses iodbc, which defines SQLWCHAR as wchar_t, as 32-bit
     (values (ffi-lib "libiodbc" '("2" #f))
             4))
    ((unix)
     (cond [(member (path->string (system-library-subpath #f))
                    '("i386-openbsd" "x86_64-openbsd"))
            ;; OpenBSD uses iodbc
            (values (ffi-lib "libiodbc" '("3.16" #f))
                    4)]
           [else
            ;; Other unixes use unixodbc, which defines WCHAR as 16-bit
            ;; for compat w/ Windows (even though Linux wchar_t is 32-bit)
            (values (ffi-lib "libodbc" '("1" #f))
                    2)]))))

(define-ffi-definer define-odbc odbc-lib)

(define (ok-status? n)
  (or (= n SQL_SUCCESS)
      (= n SQL_SUCCESS_WITH_INFO)))

(define-odbc SQLAllocHandle
  (_fun (type : _sqlsmallint)
        (parent : _sqlhandle/null)
        (handle : (_ptr o _sqlhandle/null))
        -> (status : _sqlreturn)
        -> (values status
                   (cond [handle
                          (cpointer-push-tag! handle
                                              (cond [(= type SQL_HANDLE_ENV) sqlhenv-tag]
                                                    [(= type SQL_HANDLE_DBC) sqlhdbc-tag]
                                                    [(= type SQL_HANDLE_STMT) sqlhstmt-tag]
                                                    [else sqlhandle-tag]))
                          handle]
                         [else handle]))))

;; SQLSetEnvAttr
;; must set odbc version env attr before making connection

(define-odbc SQLSetEnvAttr
  (_fun (env : _sqlhenv)
        (attr : _sqlinteger)
        (value-buf : _sqlinteger) ;; (the one case we care about takes int, not ptr)
        (_sqlinteger = 0)
        -> _sqlreturn))

(define-odbc SQLGetInfo
  (_fun (handle info) ::
        (handle : _sqlhdbc)
        (info : _sqlusmallint)
        (value : (_ptr o _sqluinteger)) ;; the one case we care about is uint, not char
        (0 : _sqlsmallint)
        (#f : _pointer)
        -> (status : _sqlreturn)
        -> (values status value)))

(define SQLGetInfo-string
  (get-ffi-obj "SQLGetInfo" odbc-lib
               (_fun (handle info) ::
                     (handle : _sqlhdbc)
                     (info : _sqlusmallint)
                     (value : _bytes = (make-bytes 250))
                     (250 : _sqlsmallint)
                     (len : (_ptr o _sqlsmallint))
                     -> (status : _sqlreturn)
                     -> (values status
                                (and (ok-status? status)
                                     (bytes->string/utf-8 value #f 0 len))))))

(define-odbc SQLGetFunctions
  (_fun (handle : _sqlhdbc)
        (function-id : _sqlusmallint)
        (supported? : (_ptr o _sqlusmallint))
        -> (status : _sqlreturn)
        -> (values status (positive? supported?))))

(define-odbc SQLConnect
  (_fun (handle server user auth) ::
        (handle : _sqlhdbc)
        (server : _string)
        ((string-utf-8-length server) : _sqlsmallint)
        (user : _string)
        ((if user (string-utf-8-length user) 0) : _sqlsmallint)
        (auth : _string)
        ((if auth (string-utf-8-length auth) 0) : _sqlsmallint)
        -> _sqlreturn))

(define-odbc SQLDriverConnect
  (_fun (handle connection driver-completion) ::
        (handle : _sqlhdbc)
        (#f : _pointer)
        (connection : _string)
        ((if connection (string-utf-8-length connection) 0) : _sqlsmallint)
        (#f : _bytes)
        (0 : _sqlsmallint)
        (out-length : (_ptr o _sqlsmallint))
        (driver-completion : _sqlusmallint)
        -> (status : _sqlreturn)
        -> status))

(define-odbc SQLBrowseConnect
  (_fun (handle in-conn-string) ::
        (handle : _sqlhdbc)
        (in-conn-string : _string)
        ((if in-conn-string (string-utf-8-length in-conn-string) 0) : _sqlsmallint)
        (out-buf : _bytes = (make-bytes 1024))
        ((bytes-length out-buf) : _sqlsmallint)
        (out-len : (_ptr o _sqlsmallint))
        -> (status : _sqlreturn)
        -> (values status
                   (and (ok-status? status)
                        (bytes->string/utf-8 out-buf #f 0 out-len)))))

(define-odbc SQLDataSources
  (_fun (handle direction server-buf descr-buf) ::
        (handle : _sqlhenv)
        (direction : _sqlusmallint)
        (server-buf : _bytes)
        ((bytes-length server-buf) : _sqlsmallint)
        (server-length : (_ptr o _sqlsmallint))
        (descr-buf : _bytes)
        ((bytes-length descr-buf) : _sqlsmallint)
        (descr-length : (_ptr o _sqlsmallint))
        -> (status : _sqlreturn)
        -> (values status
                   (and (ok-status? status)
                        (bytes->string/utf-8 server-buf #f 0 server-length))
                   (and (ok-status? status)
                        (bytes->string/utf-8 descr-buf #f 0 descr-length)))))

(define-odbc SQLDrivers
  (_fun (handle direction driver-buf attrs-buf) ::
        (handle : _sqlhenv)
        (direction : _sqlusmallint)
        (driver-buf : _bytes)
        ((bytes-length driver-buf) : _sqlsmallint)
        (driver-length : (_ptr o _sqlsmallint))
        (attrs-buf : _bytes)
        ((if attrs-buf (bytes-length attrs-buf) 0) : _sqlsmallint)
        (attrs-length : (_ptr o _sqlsmallint))
        -> (status : _sqlreturn)
        -> (if (ok-status? status)
               (values status
                       (bytes->string/utf-8 driver-buf #f 0 driver-length)
                       attrs-length)
               (values status #f #f))))

(define-odbc SQLPrepare
  (_fun (handle stmt) ::
        (handle : _sqlhstmt)
        (stmt : _string)
        ((string-utf-8-length stmt) : _sqlinteger)
        -> _sqlreturn))

(define-odbc SQLBindParameter
  (_fun (handle param-num iomode c-type sql-type column-size digits value len-or-ind) ::
        (handle : _sqlhstmt)
        (param-num : _sqlusmallint)
        (iomode : _sqlsmallint)
        (c-type : _sqlsmallint)
        (sql-type : _sqlsmallint)
        (column-size : _sqlulen)
        (digits : _sqlsmallint)
        (value : _pointer) ;; must be pinned until after SQLExecute called
        ((if (bytes? value) (bytes-length value) 0) : _sqllen) ;; ignored for fixed-length data
        (len-or-ind : _pointer) ;; _sqllen-pointer)
        -> _sqlreturn))

(define-odbc SQLExecute
  (_fun (handle : _sqlhstmt)
        -> _sqlreturn))

(define-odbc SQLNumParams
  (_fun (handle : _sqlhstmt)
        (count : (_ptr o _sqlsmallint))
        -> (status : _sqlreturn)
        -> (values status count)))

(define-odbc SQLDescribeParam
  (_fun (handle : _sqlhstmt)
        (parameter : _sqlusmallint)
        (data-type : (_ptr o _sqlsmallint))
        (size : (_ptr o _sqlulen))
        (digits : (_ptr o _sqlsmallint))
        (nullable : (_ptr o _sqlsmallint))
        -> (status : _sqlreturn)
        -> (values status data-type size digits nullable)))

(define-odbc SQLNumResultCols
  (_fun (handle : _sqlhstmt)
        (count : (_ptr o _sqlsmallint))
        -> (status : _sqlreturn)
        -> (values status count)))

(define-odbc SQLDescribeCol
  (_fun (handle column column-buf) ::
        (handle : _sqlhstmt)
        (column : _sqlusmallint)
        (column-buf : _bytes)
        (_sqlsmallint = (if column-buf (bytes-length column-buf) 0))
        (column-len : (_ptr o _sqlsmallint))
        (data-type : (_ptr o _sqlsmallint))
        (size : (_ptr o _sqlulen))
        (digits : (_ptr o _sqlsmallint))
        (nullable : (_ptr o _sqlsmallint))
        -> (status : _sqlreturn)
        -> (values status
                   (and (ok-status? status)
                        column-buf
                        (bytes->string/utf-8 column-buf #f 0 column-len))
                   data-type size digits nullable)))

(define-odbc SQLFetch
  (_fun _sqlhstmt
        -> _sqlreturn))

(define-odbc SQLGetData
  (_fun (handle column target-type buffer start) ::
        (handle : _sqlhstmt)
        (column : _sqlusmallint)
        (target-type : _sqlsmallint)
        ((ptr-add buffer start) : _gcpointer)
        ((- (bytes-length buffer) start) : _sqllen)
        (len-or-ind : (_ptr o _sqllen))
        -> (status : _sqlreturn)
        -> (values status len-or-ind)))

(define-odbc SQLGetStmtAttr/HDesc
  (_fun (handle attr) ::
        (handle : _sqlhstmt)
        (attr :   _sqlinteger)
        (valptr : (_ptr o _sqlhdesc))
        (buflen : _sqlinteger = 0)
        (strlen : _pointer = #f)
        -> (status : _sqlreturn)
        -> (and (ok-status? status) valptr))
  #:c-id SQLGetStmtAttr)

(define-odbc SQLSetDescField/Int
  (_fun (handle recno fieldid intval) ::
        (handle  : _sqlhdesc)
        (recno   : _sqlsmallint)
        (fieldid : _sqlsmallint)
        (intval  : _intptr)  ;; declared SQLPOINTER; cast
        (buflen : _sqlinteger = 0)
        -> (status : _sqlreturn))
  #:c-id SQLSetDescField)

(define-odbc SQLSetDescField/Ptr
  (_fun (handle recno fieldid ptrval buflen) ::
        (handle  : _sqlhdesc)
        (recno   : _sqlsmallint)
        (fieldid : _sqlsmallint)
        (ptrval  : _pointer)  ;; declared SQLPOINTER; cast
        (buflen : _sqlinteger)
        -> (status : _sqlreturn))
  #:c-id SQLSetDescField)

(define-odbc SQLFreeStmt
  (_fun (handle : _sqlhstmt)
        (option : _sqlusmallint)
        -> _sqlreturn))

(define-odbc SQLCloseCursor
  (_fun (handle : _sqlhstmt)
        -> _sqlreturn))

(define-odbc SQLDisconnect
  (_fun (handle : _sqlhdbc)
        -> _sqlreturn))

(define-odbc SQLFreeHandle
  (_fun (handle-type : _sqlsmallint)
        (handle : _sqlhandle)
        -> _sqlreturn))

(define-odbc SQLGetDiagRec
  (_fun (handle-type handle rec-number) ::
        (handle-type : _sqlsmallint)
        (handle : _sqlhandle)
        (rec-number : _sqlsmallint)
        (sql-state-buf : _bytes = (make-bytes 6))
        (native-errcode : (_ptr o _sqlinteger))
        (message-buf : _bytes = (make-bytes 1024))
        ((bytes-length message-buf) : _sqlsmallint)
        (message-len : (_ptr o _sqlsmallint))
        -> (status : _sqlreturn)
        -> (values status
                   (and (ok-status? status)
                        (bytes->string/utf-8 sql-state-buf #\? 0 5))
                   native-errcode
                   (and (ok-status? status)
                        (bytes->string/utf-8 message-buf #\? 0 message-len)))))

(define-odbc SQLEndTran
  (_fun (handle completion-type) ::
        (_sqlsmallint = SQL_HANDLE_DBC)
        (handle : _sqlhandle)
        (completion-type : _sqlsmallint)
        -> _sqlreturn))

(define-odbc SQLGetConnectAttr
  (_fun (handle attr) ::
        (handle : _sqlhdbc)
        (attr : _sqlinteger)
        (value : (_ptr o _sqluinteger)) ;; the attrs we care about have uint value
        (buflen : _sqlinteger = 0) ;; ignored
        (#f : _pointer)
        -> (status : _sqlreturn)
        -> (values status value)))

(define-odbc SQLSetConnectAttr
  (_fun (handle attr value) ::
        (handle : _sqlhdbc)
        (attr : _sqlinteger)
        (value : _sqluinteger) ;; the attrs we care about have uint value
        (_sqlinteger = 0)
        -> _sqlreturn))

(define-odbc SQLTables
  (_fun (handle catalog schema table) ::
        (handle : _sqlhstmt)
        (catalog : _string)
        (_sqlsmallint = (if catalog (string-utf-8-length catalog) 0))
        (schema : _string)
        (_sqlsmallint = (if schema (string-utf-8-length schema) 0))
        (table : _string)
        (_sqlsmallint = (if table (string-utf-8-length table) 0))
        (_bytes = #f)
        (_sqlsmallint = 0)
        -> _sqlreturn))
