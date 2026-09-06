#lang racket/base

(require db/private/pre
         db/unsafe/sqlite3
         rackunit)

(test-case
 "sqlite3-create-function supports NULL values"
 (define db (sqlite3-connect #:database 'memory))
 (sqlite3-create-function db "identity" 1 values)
 (check-equal? (query-value db "SELECT identity(NULL)") sql-null))
