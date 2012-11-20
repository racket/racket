#lang racket/base
(require db
         racket/format
         "main-doc.rkt")

(provide doc-db-available?
         doc-db-record-provides
         doc-db-key->path
         doc-db-file->connection)

(define (doc-db-available?) #t)

(define (doc-db-file->connection db-file)
  (sqlite3-connect #:database db-file))

(define select-pathid-vq
  (virtual-statement "SELECT pathid FROM documented WHERE stag=$1"))
(define select-path-vq
  (virtual-statement "SELECT atmain, path FROM pathids WHERE pathid=$1"))

(define (doc-db-key->path db-file key)
  (define db (if (connection? db-file)
                 db-file
                 (doc-db-file->connection db-file)))
  
  (define pathid
    (call-with-transaction/retry
     db
     (lambda ()
       (define row (query-maybe-row db 
                                    select-pathid-vq
                                    (~s key)))
       (and row
            (vector-ref row 0)))))

  (begin0
   (and pathid
        (call-with-transaction/retry
         db
         (lambda ()
           (define row (query-maybe-row db 
                                        select-path-vq
                                        pathid))
           (and row
                (let ([path (read (open-input-bytes (vector-ref row 1)))])
                  (if (equal? "y" (vector-ref row 0))
                      (main-doc-relative->path (cons 'doc path))
                      (bytes->path path)))))))
   (unless (connection? db-file)
     (disconnect db))))


(define (doc-db-record-provides db-file provides filename)
  (define filename* (path->main-doc-relative filename))
  (define filename-bytes (if (pair? filename*)
                             (string->bytes/utf-8 (~s (cdr filename*)))
                             (path->bytes filename*)))

  (define db (sqlite3-connect #:database db-file #:mode 'create))

  ;; Make sure tables are present:
  (call-with-transaction/retry
   db
   (lambda ()
     (when (null? 
            (query-rows db (~a "SELECT name FROM sqlite_master"
                               " WHERE type='table' AND name='documented'")))
       (query-exec db (~a "CREATE TABLE documented "
                          "(stag VARCHAR(256),"
                          " pathid SMALLINT,"
                          " PRIMARY KEY (stag))")))))
  (call-with-transaction/retry
   db
   (lambda ()
     (when (null? 
            (query-rows db (~a "SELECT name FROM sqlite_master"
                               " WHERE type='table' AND name='pathids'")))
       (query-exec db (~a "CREATE TABLE pathids "
                          "(pathid SMALLINT,"
                          " atmain CHAR(1),"
                          " path VARCHAR(1024),"
                          " PRIMARY KEY (pathid))")))))

  (define pathid 
    (call-with-transaction/retry
     db
     (lambda ()
       (define id (query-maybe-row db (~a "SELECT pathid FROM pathids"
                                          " WHERE atmain=$1 AND path=$2")
                                   (if (pair? filename*) "y" "n")
                                   filename-bytes))
       (cond
        [(not id)
         (define num (vector-ref (query-row db "SELECT COUNT(pathid) FROM pathids") 0))
         (query-exec db "INSERT INTO pathids VALUES ($1, $2, $3)"
                     (add1 num)
                     (if (pair? filename*) "y" "n")
                     filename-bytes)
         (add1 num)]
        [else (vector-ref id 0)]))))

  (call-with-transaction/retry
   db
   (lambda ()
     (for ([p (in-list provides)])
       (define stag (~s p))
       (query-exec db "DELETE FROM documented WHERE stag=$1"
                   stag)
       (query-exec db "INSERT INTO documented VALUES ($1, $2)"
                   stag
                   pathid))))

  (disconnect db))

(define (call-with-transaction/retry db thunk)
  (let loop ([tries 0])
    (with-handlers ([(lambda (v)
                       (and (tries . < . 100)
                            (exn:fail? v)
                            (regexp-match #rx"the database file is locked"
                                          (exn-message v))))
                     (lambda (exn)
                       ;; Try again:
                       (sleep)
                       (loop (add1 tries)))])
      (call-with-transaction
       db
       thunk))))
