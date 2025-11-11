#lang racket/base
(require racket/class
         ffi/file
         "connection.rkt"
         "dbsystem.rkt"
         "ffi.rkt")
(provide sqlite3-connect
         sqlite3-available?)

(define (sqlite3-connect #:database path0
                         #:mode [mode 'read/write]
                         #:busy-retry-delay [busy-retry-delay 0.1]
                         #:busy-retry-limit [busy-retry-limit 10]
                         #:debug? [debug? #f])
  (define who 'sqlite3-connect)
  (define path
    (case path0
      [(memory temporary) path0]
      [else (cleanse-path (path->complete-path path0))]))
  (unless (symbol? path)
    (define permissions (case mode [(read-only) '(read)] [else '(read write)]))
    (security-guard-check-file who path permissions))
  (define path-bytes
    (case path
      [(memory) #":memory:"]
      [(temporary) #""]
      [else (path->bytes path)]))
  (define db-spec (list path mode))
  (define (connect)
    (sqlite3_open_v2 path-bytes
                     (case mode
                       [(read-only) SQLITE_OPEN_READONLY]
                       [(read/write) SQLITE_OPEN_READWRITE]
                       [(create) (+ SQLITE_OPEN_READWRITE SQLITE_OPEN_CREATE)])))
  (define c
    (new connection%
         (connect connect)
         (db-spec (list path mode))
         (busy-retry-limit busy-retry-limit)
         (busy-retry-delay busy-retry-delay)))
  (when debug? (send c debug #t))
  c)

(define (sqlite3-available?)
  (and sqlite-lib #t))
