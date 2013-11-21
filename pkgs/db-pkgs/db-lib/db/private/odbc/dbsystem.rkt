#lang racket/base
(require racket/class
         db/private/generic/interfaces
         db/private/generic/common
         db/private/generic/sql-data
         db/private/generic/sql-convert)
(provide dbsystem
         field-dvec->field-info
         field-dvec->typeid
         field-dvec->size
         field-dvec->digits
         supported-typeid?
         classify-odbc-sql)

(define odbc-dbsystem%
  (class* dbsystem-base% (dbsystem<%>)
    (define/public (get-short-name) 'odbc)
    (define/override (get-type-list) type-list)
    (define/public (has-support? x) #f)

    (define/public (get-parameter-handlers param-typeids)
      (map get-check param-typeids))

    (define/public (field-dvecs->typeids dvecs)
      (map (lambda (dvec) (vector-ref dvec 1)) dvecs))

    (define/public (describe-params typeids)
      (map describe-typeid typeids))

    (define/public (describe-fields dvecs)
      (for/list ([dvec (in-list dvecs)])
        (describe-typeid (field-dvec->typeid dvec))))

    (super-new)))

(define dbsystem
  (new odbc-dbsystem%))

;; ----

(define (field-dvec->field-info dvec)
  `((name . ,(vector-ref dvec 0))
    (typeid . ,(vector-ref dvec 1))
    (size . ,(vector-ref dvec 2))
    (digits . ,(vector-ref dvec 3))))

(define (field-dvec->typeid dvec)
  (vector-ref dvec 1))
(define (field-dvec->size dvec)
  (vector-ref dvec 2))
(define (field-dvec->digits dvec)
  (vector-ref dvec 3))

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
    (let ([name (mk-check typeid (lambda (z) (or (pred z) ...)) #:contract-parts '(pred ...))] ...
          [*name *fun] ...)
      (lambda (x)
        (case x
          ((typeid) name) ...
          ((*typeid) *name) ...
          (else
           (lambda (fsym index param)
             (error/unsupported-type fsym x))))))))

(define (mk-check typeid pred #:contract-parts [ctc-parts #f])
  (lambda (fsym param)
    (unless (pred param)
      (error/no-convert fsym "ODBC" (typeid->type typeid) param
                        #:contract (cond [(= (length ctc-parts) 1)
                                          (car ctc-parts)]
                                         [else (cons 'or/c ctc-parts)])))
    param))

(define (check-numeric fsym param)
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

(define-type-table (type-list
                    typeid->type
                    describe-typeid)

  (0  unknown        0)
  (1  character      0)
  (2  numeric        0)
  (3  decimal        0)
  (4  integer        0)
  (5  smallint       0)
  (6  float          0)
  (7  real           0)
  (8  double         0)
  (9  datetime       0)
  (12 varchar        0)
  (91 date           0)
  (92 time           0)
  (93 timestamp      0)
  (-1 longvarchar    0)
  (-2 binary         0)
  (-3 varbinary      0)
  (-4 longvarbinary  0)
  (-5 bigint         0)
  (-6 tinyint        0)
  (-7 bit1           0) ;; not bit(n), always single bit
  (-8 wchar          0)
  (-9 wvarchar       0)
  (-10 wlongvarchar  0)

  ;; Unsupported types

  (101 interval-year          #f)
  (102 interval-month         #f)
  (103 interval-day           #f)
  (104 interval-hour          #f)
  (105 interval-minute        #f)
  (106 interval-second        #f)
  (107 interval-year-month    #f)
  (108 interval-day-hour      #f)
  (109 interval-day-minute    #f)
  (110 interval-day-second    #f)
  (111 interval-hour-minute   #f)
  (112 interval-hour-second   #f)
  (113 interval-minute-second #f))

(define (supported-typeid? x)
  (case x
    ((0 1 2 3 4 5 6 7 8 9 12 91 92 93 -1 -2 -3 -4 -5 -6 -7 -8 -9 -10) #t)
    (else #f)))
