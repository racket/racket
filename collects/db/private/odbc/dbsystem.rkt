#lang racket/base
(require racket/class
         "../generic/interfaces.rkt"
         "../generic/sql-data.rkt"
         "../generic/sql-convert.rkt")
(provide dbsystem
         supported-typeid?
         classify-odbc-sql)

(define odbc-dbsystem%
  (class* object% (dbsystem<%>)
    (define/public (get-short-name) 'odbc)
    (define/public (get-known-types) supported-types)
    (define/public (has-support? x) #f)

    (define/public (get-parameter-handlers param-typeids)
      (map get-check param-typeids))

    (define/public (field-dvecs->typeids dvecs)
      (map (lambda (dvec) (vector-ref dvec 1)) dvecs))

    (define/public (describe-typeids typeids)
      (map describe-typeid typeids))

    (super-new)))

(define dbsystem
  (new odbc-dbsystem%))

;; ----

;; SQL "parsing"
;; We just care about detecting commands that affect transaction status.

;; Since we have no idea what the actual database system is, just cover
;; standard commands and assume DDL is not transactional.

;; classify-odbc-sql : string [nat] -> symbol/#f
(define classify-odbc-sql
  (make-sql-classifier #:hash-comments? #t
   '(;; Explicit transaction commands
     ("ROLLBACK TRANSACTION TO"  rollback-savepoint)
     ("ROLLBACK WORK TO"  rollback-savepoint)
     ("ROLLBACK TO"       rollback-savepoint)
     ("RELEASE"           release-savepoint)
     ("SAVEPOINT"         savepoint)
     ("START"             start)
     ("BEGIN"             start)
     ("COMMIT"            commit)
     ("END"               commit)
     ("ROLLBACK"          rollback) ;; Note: after ROLLBACK TO, etc

     ;; Implicit commit
     ("ALTER"             implicit-commit)
     ("CREATE"            implicit-commit)
     ("DROP"              implicit-commit)
     ("GRANT"             implicit-commit)
     ("RENAME"            implicit-commit)
     ("TRUNCATE"          implicit-commit))))

;; ----

(define-syntax-rule
  (defchecks get-check [(typeid name pred ...) ...] [(*typeid *name *fun) ...])
  (define get-check
    (let ([name (mk-check typeid (lambda (z) (or (pred z) ...)))] ...
          [*name *fun] ...)
      (lambda (x)
        (case x
          ((typeid) name) ...
          ((*typeid) *name) ...
          (else
           (lambda (fsym index param)
             (error/unsupported-type fsym x))))))))

(define (mk-check typeid pred)
  (lambda (fsym index param)
    (unless (pred param)
      (error/no-convert fsym "ODBC" (typeid->type typeid) param))
    param))

(define (check-numeric fsym index param)
  (define (bad note)
    (error/no-convert fsym "ODBC" "numeric" param note))
  (unless (rational? param) (bad ""))
  (let ([scaled (exact->scaled-integer (inexact->exact param))])
    (unless scaled (bad ""))
    (let ([ma (car scaled)]
          [ex (cdr scaled)])
      ;; check (abs ma) fits in 16*8 bits, ex fits in char
      (unless (<= -128 ex 127) (bad "(scale too large)"))
      (unless (< (abs ma) (expt 2 (* 16 8))) (bad "(mantissa too long)"))
      (cons ma ex))))

(defchecks get-check
  [(0  unknown        string? bytes? rational? boolean? sql-date? sql-time? sql-timestamp?)
   (1  character      string?)
   (4  integer        int32?)
   (5  smallint       int16?)
   (6  float          real?)
   (7  real           real?)
   (8  double         real?)
   (9  datetime       sql-timestamp?)
   (12 varchar        string?)
   (91 date           sql-date?)
   (92 time           sql-time?)
   (93 timestamp      sql-timestamp?)
   (-1 longvarchar    string?)
   (-2 binary         bytes?)
   (-3 varbinary      bytes?)
   (-4 longvarbinary  bytes?)
   (-5 bigint         int64?)
   (-6 tinyint        int8?)
   (-7 bit1           boolean?)
   (-8 wcharacter     string?)
   (-9 wvarchar       string?)
   (-10 wlongvarchar  string?)]
  [(2  numeric        check-numeric)
   (3  decimal        check-numeric)])

;; ----

(define-type-table (supported-types
                    type-alias->type
                    typeid->type
                    type->typeid
                    describe-typeid)

  (0  unknown        ()           #t)
  (1  character      (char)       #t)
  (2  numeric        ()           #t)
  (3  decimal        ()           #t)
  (4  integer        (int)        #t)
  (5  smallint       ()           #t)
  (6  float          ()           #t)
  (7  real           ()           #t)
  (8  double         ()           #t)
  (9  datetime       ()           #t)
  (12 varchar        ()           #t)
  (91 date           ()           #t)
  (92 time           ()           #t)
  (93 timestamp      ()           #t)
  (-1 longvarchar    ()           #t)
  (-2 binary         ()           #t)
  (-3 varbinary      ()           #t)
  (-4 longvarbinary  ()           #t)
  (-5 bigint         ()           #t)
  (-6 tinyint        ()           #t)
  (-7 bit1           ()           #t) ;; not bit(n), always single bit
  (-8 wchar          ()           #t)
  (-9 wvarchar       ()           #t)
  (-10 wlongvarchar  ()           #t)

  ;; Unsupported types

  (101 interval-year          () #f)
  (102 interval-month         () #f)
  (103 interval-day           () #f)
  (104 interval-hour          () #f)
  (105 interval-minute        () #f)
  (106 interval-second        () #f)
  (107 interval-year-month    () #f)
  (108 interval-day-hour      () #f)
  (109 interval-day-minute    () #f)
  (110 interval-day-second    () #f)
  (111 interval-hour-minute   () #f)
  (112 interval-hour-second   () #f)
  (113 interval-minute-second () #f))

(define (supported-typeid? x)
  (case x
    ((0 1 2 3 4 5 6 7 8 9 12 91 92 93 -1 -2 -3 -4 -5 -6 -7 -8 -9 -10) #t)
    (else #f)))
