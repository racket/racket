#lang racket/base

(require db/private/pre
         (only-in db/private/sqlite3/connection
                  unsafe-create-function)
         racket/class
         rackunit)

(test-case
 "sqlite3-create-function supports NULL values"
 (define db (sqlite3-connect #:database 'memory))
 (send db unsafe-create-function 'sqlite3-create-function "identity" 1 values)
 (check-equal? (query-value db "SELECT identity(NULL)") sql-null))
