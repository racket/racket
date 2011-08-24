#lang racket/base
(require racket/class
         "../generic/interfaces.rkt"
         "../generic/sql-data.rkt"
         "ffi-constants.rkt")
(provide dbsystem)

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
