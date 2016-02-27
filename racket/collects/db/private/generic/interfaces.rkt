#lang racket/base
(require racket/class
         racket/list
         racket/serialize
         racket/string)
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

         log-db-error
         log-db-warning
         log-db-info
         log-db-debug

         (struct-out exn:fail:sql)
         raise-sql-error)

;; ----------------------------------------

;; Interfaces

;; connection<%>
(define connection<%>
  (interface ()
    ;; connected? method must return promptly (eg, without acquiring lock)
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
    start-transaction  ;; symbol (U 'serializable ...) any boolean -> void
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
    get-known-types        ;; real -> (listof symbol)
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
(serializable-struct simple-result (info) #:transparent)
(serializable-struct rows-result (headers rows) #:transparent)

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

(define-logger db)

;; ----------------------------------------

;; Exceptions

#|
Only errors with an associated SQLSTATE are represented by
exn:fail:sql, specifically only errors originating from a database
backend or library. Other errors are typically raised using 'error',
producing plain old exn:fail.

For SQLite, use symbol instead of SQLSTATE string.
|#

;; exn:fail:sql
;; Represents an error with an associated SQLSTATE
(define-struct (exn:fail:sql exn:fail) (sqlstate info))

;; raise-sql-error : symbol string string alist -> raises exn
(define (raise-sql-error who sqlstate message info)
  (raise
   (make-exn:fail:sql (compose-error-message who message
                                             "SQLSTATE" sqlstate)
                      (current-continuation-marks)
                      sqlstate
                      info)))

;; ----------------------------------------

;; Common Errors

(provide error/internal
         error/internal*
         error/not-connected
         error/disconnect-in-lock
         error/no-support
         error/need-password
         error/comm
         error/hopeless
         error/unsupported-type
         error/no-convert
         error/invalid-nested-isolation
         error/tx-bad-stmt
         error/unbalanced-tx
         error/unclosed-tx
         error/nested-tx-option
         error/exn-in-rollback
         error/stmt-arity
         error/stmt
         error/want-rows
         error/want-cursor
         error/column-count
         error/row-count
         error/statement-binding-args
         ;; other modules also define some error reporting
         compose-error-message
         error*)

(define (error* who message
                #:continued [continued-message null]
                . field+detail-list)
  (raise
   (exn:fail
    (compose* who message
              continued-message
              (field+detail-list->table 'error* field+detail-list null))
    (current-continuation-marks))))

;; compose-error-message : .... -> string
(define (compose-error-message who message
                               #:continued [continued-message null]
                               . field+detail-list)
  (define details
    (field+detail-list->table 'compose-error-message field+detail-list null))
  (compose* who message continued-message details))

(define (compose* who message continued-message details)
  (let* ([parts (apply append
                       (for/list ([detail (in-list details)])
                         (let* ([field+opts (car detail)]
                                [field (if (pair? field+opts) (car field+opts) field+opts)]
                                [options (if (pair? field+opts) (cdr field+opts) '())]
                                [value (cdr detail)])
                           (compose-detail* field options value))))]
         [parts (let loop ([continued continued-message])
                  (cond [(pair? continued) (list* "\n " (car continued) (loop (cdr continued)))]
                        [(string? continued) (loop (list continued))]
                        [(null? continued) parts]))]
         [parts (list* message (if (null? continued-message) "" ";") parts)]
         [parts (if who
                    (list* (symbol->string who) ": " parts)
                    parts)])
    (apply string-append parts)))

(define (compose-detail* field options value)
  (let* ([value? (memq 'value options)]
         [multi? (memq 'multi options)]
         [maybe? (memq 'maybe options)]
         [noindent? (memq 'noindent options)]
         [convert-value0
          (cond [value?
                 (lambda (v) ((error-value->string-handler) v (error-print-width)))]
                [else
                 (lambda (v) (format "~a" v))])]
         [convert-value
          (if noindent?
              (lambda (v indent) (list (convert-value0 v)))
              (lambda (v indent)
                (let* ([s (convert-value0 v)]
                       [lines (string-split s #rx"[\n]" #:trim? #f)]
                       [spacing
                        (case indent
                          ((3) "\n   ") ;; common case, make constant
                          (else (string-append "\n" (make-string indent #\space))))])
                  (add-between lines spacing))))])
    (cond [(and (or maybe? multi? (not value?))
                (not value))
           null]
          [(and maybe? multi?
                (null? value))
           null]
          [multi?
           (list* "\n  " field ": "
                  (let value-loop ([value value])
                    (cond [(pair? value)
                           (list* "\n   "
                                  (append (convert-value (car value) 3)
                                          (value-loop (cdr value))))]
                          [(null? value)
                           null])))]
          [else
           (list* "\n  " field ": "
                  (convert-value value (+ 4 (string-length field))))])))

(define (field+detail-list->table who lst onto)
  (cond [(null? lst) onto]
        [else
         (let ([field (car lst)]
               [value (cadr lst)])
           (cons (cons field value)
                 (field+detail-list->table who (cddr lst) onto)))]))


(define (error/internal fsym fmt . args)
  (error* fsym "internal error"
          #:continued (apply format fmt args)))

(define (error/internal* fsym msg . args)
  (apply error* fsym "internal error" #:continued msg args))

;; FIXME; clean up
(define (error/comm fsym [when-occurred #f])
  (error* fsym "communication failure"
          "when" when-occurred))

(define (error/no-support fsym feature)
  (error* fsym "feature not supported"
          "feature" feature))

(define (error/hopeless fsym)
  (error fsym "connection is permanently locked due to a terminated thread"))

(define (error/not-connected fsym)
  (error fsym "not connected"))

(define (error/disconnect-in-lock fsym)
  (error fsym "disconnected during operation;\n possibly due to custodian shutdown"))

;; ----

(define (error/invalid-nested-isolation fsym isolation)
  (error* fsym "invalid isolation level for nested transaction"
          '("isolation level" value) isolation))

(define (error/unbalanced-tx fsym mode saved-cwt?)
  (error fsym "~a-transaction without matching start-transaction~a"
         mode (if saved-cwt? " (within the extent of call-with-transaction)" "")))

(define (error/unclosed-tx fsym mode saved-cwt?)
  (error fsym "unclosed nested transaction~a"
         (if saved-cwt? " (within extent of call-with-transaction)" "")))

(define (error/tx-bad-stmt fsym stmt-type-string tx-state)
  (error* fsym "statement not allowed in current transaction state"
          "statement type" stmt-type-string
          "transaction state" tx-state))

(define (error/nested-tx-option fsym option)
  (error* fsym "option not allowed for nested transaction"
          '("option" value) option))

(define (error/exn-in-rollback fsym e1 e2)
  (error* fsym "error during rollback"
          #:continued "secondary error occurred during rollback triggered by primary error"
          '("primary" value) (exn-message e1)
          '("secondary" value) (exn-message e2)))

;; ----

(define (error/stmt-arity fsym expected given)
  (error* fsym "wrong number of parameters for query"
                    ;; FIXME: add stmt, use error/stmt
                    "expected" expected
                    "given" given))

;; ----

(define (error/need-password fsym)
  (error fsym "password needed but not supplied"))

;; ----

(define (error/unsupported-type fsym typeid [type #f])
  (error* fsym "unsupported type"
          "type" type
          "typeid" typeid))

(define (error/no-convert fsym sys type param [note #f] #:contract [ctc #f])
  (error* fsym "cannot convert given value to SQL type"
          '("given" value) param
          "type" type
          "expected" (and ctc (format "~.s" ctc))
          "dialect" sys
          "note" note))

;; ----

(define (error/stmt fsym stmt message . args)
  (apply error* fsym message
         '("statement" value) (or (let loop ([stmt stmt])
                                    (cond [(string? stmt) stmt]
                                          [(statement-binding? stmt) (loop (statement-binding-pst stmt))]
                                          [(prepared-statement? stmt) (loop (send stmt get-stmt))]
                                          [else #f]))
                                  stmt)
         ;; FIXME: include params from statement-binding values?
         ;; must first change statement-binding to store raw params
         args))

(define (error/want-rows fsym sql executed?)
  (error/stmt fsym sql
              (if executed?
                  "query did not return rows"
                  "query does not return rows")))

(define (error/want-cursor fsym sql)
  (error/stmt fsym sql "query did not return cursor"))

(define (error/column-count fsym sql want-columns got-columns executed?)
  (error/stmt fsym sql
              (if executed?
                  "query returned wrong number of columns"
                  "query returns wrong number of columns")
              "expected" want-columns
              "got" got-columns))

(define (error/row-count fsym sql want-rows got-rows)
  (error/stmt fsym sql "query returned wrong number of rows"
              "expected" want-rows
              "got" got-rows))

(define (error/statement-binding-args fsym stmt args)
  (error* fsym
          "cannot execute statement-binding with additional inline arguments"
          '("statement" value) stmt
          '("arguments" value) args))
