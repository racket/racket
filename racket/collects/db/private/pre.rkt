#lang racket/base

;; Minimal db/base and db/sqlite3, used by core Racket (pre-pkg)

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
(provide connected?
         disconnect
         virtual-statement
         (rename-out [query-rows0 query-rows])
         query-list
         query-row
         query-maybe-row
         query-value
         query-maybe-value
         query-exec
         query
         start-transaction
         commit-transaction
         rollback-transaction
         call-with-transaction
         in-transaction?
         needs-rollback?)

(require "sqlite3/main.rkt")
(provide sqlite3-connect
         sqlite3-available?)
