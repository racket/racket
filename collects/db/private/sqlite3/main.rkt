#lang racket/base
(require racket/class
         racket/contract
         ffi/file
         "connection.rkt"
         "dbsystem.rkt"
         "ffi.rkt")
(provide sqlite3-connect
         (rename-out [dbsystem sqlite3-dbsystem]))

(define (sqlite3-connect #:database path-or-sym
                         #:mode [mode 'read/write]
                         #:busy-retry-delay [busy-retry-delay 0.1]
                         #:busy-retry-limit [busy-retry-limit 10])
  (let ([path
         (cond [(symbol? path-or-sym)
                (case path-or-sym
                  ;; Private, temporary in-memory
                  [(memory) #":memory:"]
                  ;; Private, temporary on-disk
                  [(temporary) #""])]
               [(or (path? path-or-sym) (string? path-or-sym))
                (let ([path (cleanse-path (path->complete-path path-or-sym))])
                  (security-guard-check-file 'sqlite3-connect
                                             path
                                             (case mode
                                               ((read-only) '(read))
                                               (else '(read write))))
                  (path->bytes path))])])
    (let-values ([(db open-status)
                  (sqlite3_open_v2 path
                                   (case mode
                                     ((read-only) SQLITE_OPEN_READONLY)
                                     ((read/write) SQLITE_OPEN_READWRITE)
                                     ((create)
                                      (+ SQLITE_OPEN_READWRITE SQLITE_OPEN_CREATE))))])
      (handle-status* 'sqlite3-connect open-status db)
      (new connection%
           (db db)
           (busy-retry-limit busy-retry-limit)
           (busy-retry-delay busy-retry-delay)))))
