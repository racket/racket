#lang racket/base

;; Most of db/base and db/sqlite3, used by core Racket (pre-pkg)

(require "generic/interfaces.rkt")
(provide (struct-out simple-result)
         (struct-out rows-result)
         statement-binding?
         (struct-out exn:fail:sql)
         connection?
         dbsystem?
         prepared-statement?)

(require "generic/sql-data.rkt")
(provide sql-null
         sql-null?)

(require "generic/functions.rkt")
(provide (except-out (all-from-out "generic/functions.rkt")
                     in-query-helper))

(require "sqlite3/main.rkt")
(provide sqlite3-connect
         sqlite3-available?)
