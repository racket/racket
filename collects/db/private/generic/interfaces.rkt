#lang racket/base
(require racket/class)
(provide connection<%>
         dbsystem<%>
         prepared-statement<%>

         connection?
         dbsystem?
         prepared-statement?

         (struct-out statement-binding)

         (struct-out simple-result)
         (struct-out rows-result)
         (struct-out cursor-result)

         init-private

         dblogger
         dbdebug

         (struct-out exn:fail:sql)
         raise-sql-error)

;; ----------------------------------------

;; Interfaces

;; connection<%>
(define connection<%>
  (interface ()
    connected?    ;; -> boolean
    disconnect    ;; -> void
    get-dbsystem  ;; -> dbsystem<%>
    query         ;; symbol statement -> QueryResult
    prepare       ;; symbol preparable boolean -> prepared-statement<%>
    fetch/cursor  ;; symbol cursor nat -> #f or (listof vector)
    get-base      ;; -> connection<%> or #f (#f means base isn't fixed)
    list-tables   ;; symbol symbol -> (listof string)

    ;; in start-tx and end-tx, the final boolean arg indicates whether the
    ;; transaction is managed manually (#f) or by call-with-tx (#t)
    start-transaction  ;; symbol (U 'serializable ...) boolean -> void
    end-transaction    ;; symbol (U 'commit 'rollback) boolean -> void
    transaction-status ;; symbol -> (U boolean 'invalid)
    free-statement))   ;; prepared-statement<%> boolean -> void

;; dbsystem<%>
;; Represents brand of database system, SQL dialect, etc
(define dbsystem<%>
  (interface ()
    get-short-name         ;; -> symbol

    get-parameter-handlers ;; (listof typeid) -> (listof ParameterHandler)
    field-dvecs->typeids   ;; (listof field-dvec) -> (listof typeid)

    ;; inspection only
    get-known-types        ;; -> (listof symbol)
    describe-params        ;; (listof typeid) -> (listof TypeDesc)
    describe-fields))      ;; (listof field-dvec) -> (listof TypeDesc)

;; ParameterHandler = (fsym index datum -> ???)
;; Each system gets to choose its checked-param representation.
;; Maybe check and convert to string. Maybe just check, do binary conversion later.

;; TypeDesc = (list boolean symbol/#f typeid)

;; prepared-statement<%>
(define prepared-statement<%>
  (interface ()
    get-handle         ;; -> Handle (depends on database system)
    set-handle         ;; Handle -> void

    get-close-on-exec? ;; -> boolean
    after-exec         ;; boolean -> void (for close-on-exec)

    get-stmt           ;; -> string/#f
    get-stmt-type      ;; -> symbol/#f

    get-param-count    ;; -> nat
    get-param-typeids  ;; -> (listof typeid)

    get-result-dvecs   ;; -> (listof field-dvec)
    get-result-count   ;; -> nat
    get-result-typeids ;; -> (listof typeid)

    check-owner        ;; symbol connection any -> #t (or error)
    bind               ;; symbol (listof param) -> statement-binding

    ;; inspection only
    get-param-types    ;; -> (listof TypeDesc)
    get-result-types   ;; -> (listof TypeDesc)
    ))

(define (connection? x)
  (is-a? x connection<%>))

(define (dbsystem? x)
  (is-a? x dbsystem<%>))

(define (prepared-statement? x)
  (is-a? x prepared-statement<%>))

;; ----------------------------------------

;; Auxiliary structures

;; A statement-binding is:
;;   - (statement-binding prepared-statement (listof ???))
(struct statement-binding (pst params))

;; An query-result is one of:
;;  - (simple-result alist)
;;  - (rows-result Header data)
;;    for user-visible rows-results: headers present, data is (listof vector)
(struct simple-result (info) #:transparent)
(struct rows-result (headers rows) #:transparent)

;; A cursor-result is
;;  - (cursor-result Header prepared-statement ???)
(struct cursor-result (headers pst extra))

;; A Header is (listof FieldInfo)
;; A FieldInfo is an alist, contents dbsys-dependent

;; ----------------------------------------

;; Class utilities

;; Here just because ...

(define-syntax-rule (init-private iid ...)
  (begin (init-private1 iid) ...))

(define-syntax-rule (init-private1 iid)
  (begin (init ([private-iid iid]))
         (define iid private-iid)))

;; ----------------------------------------

;; Logging

(define dblogger (make-logger 'db (current-logger)))

(define (dbdebug fmt . args)
  (log-message dblogger 'debug (apply format fmt args) #f))

;; ----------------------------------------

;; Exceptions

#|
Only errors with an associated SQLSTATE are represented by
exn:fail:sql, specifically only errors originating from a database
backend or library. Other errors are typically raised using 'error',
producing plain old exn:fail.
|#

;; exn:fail:sql
;; Represents an error with an associated SQLSTATE
(define-struct (exn:fail:sql exn:fail) (sqlstate info))

;; raise-sql-error : symbol string string alist -> raises exn
(define (raise-sql-error who sqlstate message info)
  (raise 
   (make-exn:fail:sql (format "~a: ~a (SQLSTATE ~a)" who message sqlstate)
                      (current-continuation-marks)
                      sqlstate
                      info)))

;; ----------------------------------------

;; Common Errors

(provide uerror
         error/internal
         error/not-connected
         error/need-password
         error/comm
         error/hopeless
         error/unsupported-type
         error/no-convert
         error/unbalanced-tx
         error/unclosed-tx)

;;(define uerror raise-user-error)
(define uerror error)

(define (error/internal fsym fmt . args)
  (apply error fsym (string-append "internal error: " fmt) args))

(define (error/not-connected fsym)
  (uerror fsym "not connected"))

(define (error/need-password fsym)
  (uerror fsym "password needed but not supplied"))

(define (error/comm fsym [when-occurred #f])
  (if when-occurred
      (error/internal fsym "communication problem ~a" when-occurred)
      (error/internal fsym "communication problem")))

(define (error/hopeless fsym)
  (uerror fsym "connection is permanently locked due to a terminated thread"))

(define (error/unsupported-type fsym typeid [type #f])
  (if type
      (uerror fsym "unsupported type: ~a (typeid ~a)" type typeid)
      (uerror fsym "unsupported type: (typeid ~a)" typeid)))

(define (error/no-convert fsym sys type param [note #f])
  (uerror fsym "cannot convert to ~a ~a type~a~a: ~e"
          sys type (if note " " "") (or note "") param))

(define (error/unbalanced-tx fsym mode saved-cwt?)
  (error fsym "~a-transaction without matching start-transaction~a"
         mode (if saved-cwt? " (within the extent of call-with-transaction)" "")))

(define (error/unclosed-tx fsym mode saved-cwt?)
  (error fsym "unclosed nested transaction~a"
         (if saved-cwt? " (within extent of call-with-transaction)" "")))
