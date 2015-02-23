#lang racket/base
(require racket/class
         ffi/file
         "connection.rkt"
         "dbsystem.rkt"
         "ffi.rkt")
(provide sqlite3-connect
         sqlite3-available?)

(define (sqlite3-connect #:database path
                         #:mode [mode 'read/write]
                         #:busy-retry-delay [busy-retry-delay 0.1]
                         #:busy-retry-limit [busy-retry-limit 10]
                         #:debug? [debug? #f])
  (let* ([path
          (case path
            ((memory temporary) path)
            (else
             (let ([path (cleanse-path (path->complete-path path))])
               (security-guard-check-file 'sqlite3-connect
                                          path
                                          (case mode
                                            ((read-only) '(read))
                                            (else '(read write))))
               path)))]
         [path-bytes
          (case path
            ((memory) #":memory:")
            ((temporary) #"")
            (else (path->bytes path)))])
    (let-values ([(db open-status)
                  (sqlite3_open_v2 path-bytes
                                   (case mode
                                     ((read-only) SQLITE_OPEN_READONLY)
                                     ((read/write) SQLITE_OPEN_READWRITE)
                                     ((create)
                                      (+ SQLITE_OPEN_READWRITE SQLITE_OPEN_CREATE))))])
      (handle-status* 'sqlite3-connect open-status db (list path mode) #f)
      (let ([c
             (new connection%
                  (db db)
                  (db-spec (list path mode))
                  (busy-retry-limit busy-retry-limit)
                  (busy-retry-delay busy-retry-delay))])
        (when debug? (send c debug #t))
        c))))

(define (sqlite3-available?)
  (and sqlite-lib #t))
