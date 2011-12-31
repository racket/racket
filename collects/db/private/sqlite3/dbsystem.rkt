#lang racket/base
(require racket/class
         "../generic/interfaces.rkt")
(provide dbsystem
         classify-sl-sql)

(define sqlite3-dbsystem%
  (class* object% (dbsystem<%>)
    (define/public (get-short-name) 'sqlite3)
    (define/public (get-known-types) '(any))
    (define/public (has-support? x) #f)

    (define/public (get-parameter-handlers param-typeids)
      (map (lambda (param-typeid) check-param)
           param-typeids))

    (define/public (field-dvecs->typeids dvecs)
      (map (lambda (dvec) (vector-ref dvec 0))
           dvecs))

    (define/public (describe-typeids typeids)
      (map (lambda _ '(#t any #f))
           typeids))

    (super-new)))

(define dbsystem
  (new sqlite3-dbsystem%))

;; ========================================

(define (check-param fsym index param)
  (unless (or (real? param)
              (string? param)
              (bytes? param))
    (error/no-convert fsym "SQLite" "parameter" param))
  param)

;; ========================================


;; SQL "parsing"
;; We just care about detecting commands that affect transaction status.

;; classify-sl-sql : string [nat] -> symbol/#f
(define classify-sl-sql
  (make-sql-classifier
   '(;; Explicit transaction commands
     ("ROLLBACK TRANSACTION TO"  rollback-savepoint)
     ("ROLLBACK TO"       rollback-savepoint)
     ("RELEASE"           release-savepoint)
     ("SAVEPOINT"         savepoint)
     ;; Note: SAVEPOINT allowed outside of transaction! (but that's okay)

     ("BEGIN"             start)
     ("COMMIT"            commit)
     ("END"               commit)
     ("ROLLBACK"          rollback) ;; Note: after ROLLBACK TO, etc
     )))
