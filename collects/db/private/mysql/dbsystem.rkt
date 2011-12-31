#lang racket/base
(require racket/class
         "../generic/interfaces.rkt"
         "../generic/sql-data.rkt"
         "../../util/private/geometry.rkt"
         (only-in "message.rkt" field-dvec->typeid))
(provide dbsystem
         classify-my-sql)

(define mysql-dbsystem%
  (class* object% (dbsystem<%>)

    (define/public (get-short-name) 'mysql)
    (define/public (get-known-types) supported-types)

    (define/public (has-support? option)
      (case option
        ((real-infinities) #f)
        ((numeric-infinities) #f)
        (else #f)))

    (define/public (get-parameter-handlers param-typeids)
      ;; All params sent as binary data, so handled in message.rkt
      ;; Just need to check params for legal values here
      ;; FIXME: for now, only possible param type is var-string;
      ;; when that changes, will need to refine check-param.
      (map (lambda (param-typid) check-param)
           param-typeids))

    (define/public (field-dvecs->typeids dvecs)
      (map field-dvec->typeid dvecs))

    (define/public (describe-typeids typeids)
      (map describe-typeid typeids))

    (super-new)))

(define dbsystem
  (new mysql-dbsystem%))


;; ========================================

(define (check-param fsym index param)
  (unless (or (string? param)
              (rational? param)
              (bytes? param)
              (sql-date? param)
              (sql-time? param)
              (sql-timestamp? param)
              (sql-day-time-interval? param)
              (sql-bits? param)
              (geometry2d? param))
    (error/no-convert fsym "MySQL" "parameter" param))
  param)

;; ========================================

;; SQL "parsing"
;; We care about:
;;  - determining whether commands must be prepared (to use binary data)
;;    see http://dev.mysql.com/doc/refman/5.0/en/c-api-prepared-statements.html
;;  - detecting commands that affect transaction status (maybe implicitly)
;;    see http://dev.mysql.com/doc/refman/5.0/en/implicit-commit.html

;; classify-my-sql : string [nat] -> symbol/#f
(define classify-my-sql
  (make-sql-classifier #:hash-comments? #t
   '(;; Must be prepared
     ("SELECT"            select)
     ("SHOW"              show)

     ;; Explicit transaction commands
     ("ROLLBACK WORK TO"  rollback-savepoint)
     ("ROLLBACK TO"       rollback-savepoint)
     ("RELEASE SAVEPOINT" release-savepoint)
     ("SAVEPOINT"         savepoint)
     ("START TRANSACTION" start)
     ("BEGIN"             start)
     ("COMMIT"            commit)
     ("ROLLBACK"          rollback) ;; Note: after ROLLBACK TO, etc
     ("SET autocommit"    set-autocommit) ;; trouble
     ;; Note: commit/rollback may immediately start new transaction

     ;; Implicit commit
     ("ALTER"             implicit-commit)
     ("CREATE"            implicit-commit)
     ("DROP"              implicit-commit)
     ("RENAME"            implicit-commit)
     ("TRUNCATE"          implicit-commit)
     ("LOAD"              implicit-commit)
     ("LOCK TABLES"       implicit-commit)
     ("UNLOCK TABLES"     implicit-commit))))

;; ========================================

(define-type-table (supported-types
                    type-alias->type
                    typeid->type
                    type->typeid
                    describe-typeid)

  (newdecimal  decimal     ()    #t)
  (tiny        tinyint     ()    #t)
  (short       smallint    ()    #t)
  (int24       mediumint   ()    #t)
  (long        integer     (int) #t)
  (longlong    bigint      ()    #t)
  (float       real        ()    #t)
  (double      double      ()    #t)
  (newdate     date        ()    #t)
  (time        time        ()    #t)
  (datetime    datetime    ()    #t)
  (varchar     varchar     ()    #t)
  (var-string  var-string  ()    #t)
  (tiny-blob   tinyblob    ()    #t)
  (medium-blob mediumblob  ()    #t)
  (long-blob   longblob    ()    #t)
  (blob        blob        ()    #t)
  (bit         bit         ()    #t)
  (geometry    geometry    ()    #t))

;; decimal, date typeids not used (?)
