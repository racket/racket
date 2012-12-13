#lang racket/base
(require db
         racket/format
         racket/serialize
         "main-doc.rkt")

(provide doc-db-available?
         doc-db-clear-provides
         doc-db-add-provides
         doc-db-clear-dependencies
         doc-db-add-dependencies
         doc-db-clear-searches
         doc-db-add-searches
         doc-db-key->path
         doc-db-check-duplicates
         doc-db-check-unsatisfied
         doc-db-get-dependencies
         doc-db-file->connection
         doc-db-disconnect
         doc-db-clean-files
         doc-db-init-pause
         doc-db-pause
         log-doc-db-info)

(define-logger doc-db)

(define (doc-db-available?)
  (sqlite3-available?))

(define (doc-db-file->connection db-file)
  (define exists? (file-exists? db-file))
  (define db (sqlite3-connect #:database db-file 
                              #:mode 'create
                              #:busy-retry-limit 0))
  (unless exists?
    (call-with-retry/transaction
     'prepare-tables
     db
     #t
     (lambda ()
       (prepare-tables db))))
  ;; we don't need to survive a catastrophic failure:
  (call-with-retry
   'synchronous-off
   db
   (lambda ()
     (query-exec db "pragma synchronous = off")))
  db)

(define (doc-db-disconnect db)
  (disconnect db))

(define select-pathid-vq
  (virtual-statement "SELECT pathid FROM documented WHERE stag=$1"))
(define select-path-vq
  (virtual-statement "SELECT atmain, path FROM pathids WHERE pathid=$1"))
(define select-other-path-vq
  (virtual-statement "SELECT atmain, path FROM other.pathids WHERE pathid=$1"))

(define (doc-db-init-pause)
  0.01)
(define (doc-db-pause who pause)
  (log-doc-db-info "database locked at ~a; now waiting ~a seconds" 
                   who
                   pause)
  (sleep pause)
  (min 2 (* 2 pause)))

(define (call-with-database who db-file proc 
                            #:write? [write? #f]
                            #:setup [setup void]
                            #:teardown [teardown void])
  (define db (if (connection? db-file)
                 db-file
                 (doc-db-file->connection db-file)))
  (setup db)
  (begin0
   (call-with-retry/transaction
    who
    db
    write?
    (lambda () (proc db)))
   (teardown db)
   (unless (connection? db-file)
     (disconnect db))))

(define (doc-db-key->path db-file key
                          #:main-doc-relative-ok? [main-doc-relative-ok? #f])
  (call-with-database
   'doc-db-key->path
   db-file
   (lambda (db)
     (define rows (query-rows db select-pathid-vq
                              (->string key)))
     (define pathid (and (pair? rows)
                         (vector-ref (car rows) 0)))
     (and pathid
          (pathid->filename db pathid #f main-doc-relative-ok?)))))


(define (add who db-file elems filename callback)
  (call-with-database
   who
   db-file
   #:write? #t
   (lambda (db)
     (prepare-tables db)
     (define pathid (filename->pathid db filename))
     (for ([p (in-list elems)])
       (define stag (->string p))
       (callback db stag pathid)))))

(define (clear who db-file filename statement)
  (call-with-database
   who
   db-file
   #:write? #t
   (lambda (db)
     (prepare-tables db)
     (define pathid (filename->pathid db filename))
     (query-exec db statement
                 pathid))))

(define (doc-db-add-provides db-file provides filename)
  (add 'doc-db-add-provides
       db-file provides filename
       (lambda (db stag pathid)
         (query-exec db "INSERT INTO documented VALUES ($1, $2)"
                     stag
                     pathid))))
       

(define (doc-db-clear-provides db-file filename)
  (clear 'doc-db-clear-provides
         db-file filename
         "DELETE FROM documented WHERE pathid=$1"))

(define (doc-db-add-dependencies db-file depends filename)
  (add 'doc-db-add-dependencies
       db-file depends filename
       (lambda (db stag pathid)
         (query-exec db "INSERT INTO dependencies VALUES ($1, $2)"
                     pathid
                     stag))))


(define (doc-db-clear-dependencies db-file filename)
  (clear 'doc-db-clear-dependencies
         db-file filename 
         "DELETE FROM dependencies WHERE pathid=$1"))

(define (doc-db-add-searches db-file searches filename)
  (call-with-database
   'doc-db-add-searches
   db-file
   #:write? #t
   (lambda (db)
     (prepare-tables db)
     (define pathid (filename->pathid db filename))
     (for ([(sk s) (in-hash searches)]
           [setid (in-naturals)])
       (query-exec db "INSERT INTO searchSets VALUES ($1, $2, $3)"
                   pathid
                   setid
                   (->string sk))
       (for ([k (in-hash-keys s)])
         (define stag (->string k))
         (query-exec db "INSERT INTO searches VALUES ($1, $2, $3)"
                     pathid
                     setid
                     stag))))))

(define (doc-db-clear-searches db-file filename)
  (call-with-database
   'doc-db-clear-searches
   db-file
   #:write? #t
   (lambda (db)
     (prepare-tables db)
     (define pathid (filename->pathid db filename))
     (query-exec db "DELETE FROM searchSets WHERE pathid=$1"
                 pathid)
     (query-exec db "DELETE FROM searches WHERE pathid=$1"
                 pathid))))

(define (maybe-attach attach-db-path)
  (lambda (db)
    (when attach-db-path
      (attach-db db attach-db-path))))
(define (maybe-detach attach-db-path)
  (lambda (db)
    (when attach-db-path
      (detach-db db attach-db-path))))

(define (doc-db-check-duplicates db-file
                                 #:attach [attach-db-path #f]
                                 #:main-doc-relative-ok? [main-doc-relative-ok? #f])
  (call-with-database
   'doc-db-check-duplicates
   db-file
   #:setup (maybe-attach attach-db-path)
   #:teardown (maybe-detach attach-db-path)
   (lambda (db)
     (define rows
       (append
        (query-rows db (~a "SELECT stag"
                           " FROM documented"
                           " GROUP BY stag"
                           " HAVING COUNT(pathid) > 1"))
        (if attach-db-path
            ;; duplicates across tables:
            (query-rows db (~a "SELECT D.stag"
                               " FROM documented D, other.documented OD"
                               " WHERE D.stag = OD.stag"
                               " GROUP BY D.stag"))
            null)))
     (for/list ([row (in-list rows)])
       (define stag (vector-ref row 0))
       (define pathid-rows (query-rows db (~a "SELECT pathid"
                                              " FROM documented"
                                              " WHERE stag=$1")
                                       stag))
       (define other-pathid-rows 
         (if attach-db-path
             (query-rows db (~a "SELECT pathid"
                                " FROM other.documented"
                                " WHERE stag=$1")
                         stag)
             null))
       (cons (<-string (if (bytes? stag)
                           (open-input-bytes stag)
                           (open-input-string stag)))
             (append
              (for/list ([pathid-row (in-list pathid-rows)])
                (pathid->filename db (vector-ref pathid-row 0) #f main-doc-relative-ok?))
              (for/list ([pathid-row (in-list other-pathid-rows)])
                (pathid->filename db (vector-ref pathid-row 0) #t main-doc-relative-ok?))))))))

(define (doc-db-check-unsatisfied filename db-file
                                  #:attach [attach-db-path #f])
  (call-with-database
   'doc-db-check-unsatisfied
   db-file
   #:setup (maybe-attach attach-db-path)
   #:teardown (maybe-detach attach-db-path)
   (lambda (db)
     (define pathid (filename->pathid db filename))
     ;; Items with no `searches' entries:
     (define rows
       (query-rows db (~a "SELECT P.stag "
                          " FROM dependencies P"
                          " LEFT OUTER JOIN documented D ON D.stag = P.stag"
                          " LEFT OUTER JOIN searches S ON S.stag = P.stag"
                          (if attach-db-path
                              (~a " LEFT OUTER JOIN other.documented OD ON OD.stag = P.stag"
                                  " LEFT OUTER JOIN other.searches OS ON OS.stag = P.stag")
                              "")
                          " WHERE P.pathid = $1"
                          " AND   D.stag IS NULL"
                          " AND   S.stag is NULL"
                          (if attach-db-path
                              (~a " AND   OD.stag IS NULL"
                                  " AND   OS.stag is NULL")
                              ""))
                   pathid))
     ;; Items with `searches' entries, but no documentation:
     (define more-rows
       (query-rows db (~a "SELECT SS.stag "
                          " FROM searchSets SS"
                          " WHERE SS.pathid = $1"
                          " AND NOT EXISTS"
                          " (SELECT S.stag"
                          "  FROM documented D, searches S"
                          "  WHERE D.stag = S.stag"
                          "  AND   S.setid = SS.setid"
                          "  AND   S.pathid = SS.pathid)"
                          (if attach-db-path
                              (~a " AND NOT EXISTS"
                                  " (SELECT S.stag"
                                  "  FROM other.documented OD, searches S"
                                  "  WHERE OD.stag = S.stag"
                                  "  AND   S.setid = SS.setid"
                                  "  AND   S.pathid = SS.pathid)")
                              "")
                          " GROUP BY SS.stag")
                   pathid))
     (map (lambda (s)
            (<-string (open-input-string (vector-ref s 0))))
          (append
           rows
           more-rows)))))

(define (attach-db db attach-db-path)
  (query-exec db "ATTACH $1 AS other"
              (path->bytes (cleanse-path 
                            (path->complete-path attach-db-path)))))
(define (detach-db db attach-db-path)
  (query-exec db "DETACH other"))
               

(define (doc-db-get-dependencies filename db-file
                                 #:attach [attach-db-path #f]
                                 #:main-doc-relative-ok? [main-doc-relative-ok? #f])
  (call-with-database
   'doc-db-get-dependencies
   db-file
   #:setup (maybe-attach attach-db-path)
   #:teardown (maybe-detach attach-db-path)
   (lambda (db)
     (define pathid (filename->pathid db filename))
     (define ((rows->paths in-other?) rows)
       (for/list ([row (in-list rows)])
         (pathid->filename db (vector-ref row 0) in-other? main-doc-relative-ok?)))
     (append
      ((rows->paths #f)
       (query-rows db (~a "SELECT D.pathid "
                          " FROM dependencies P, documented D"
                          " WHERE P.pathid = $1"
                          "   AND D.stag = P.stag"
                          " GROUP BY D.pathid")
                   pathid)))
     (if attach-db-path
         ((rows->paths #t)
          (query-rows db (~a "SELECT D.pathid "
                             " FROM dependencies P, other.documented D"
                             " WHERE P.pathid = $1"
                             "   AND D.stag = P.stag"
                             " GROUP BY D.pathid")
                      pathid))
         null))))

(define (doc-db-clean-files db-file ok-files)
  (call-with-database
   'doc-db-clean-files
   db-file
   #:write? #t
   (lambda (db)
     (prepare-tables db)
     (define rows (query-rows db "SELECT atmain, path, pathid FROM pathids"))
     (for ([row (in-list rows)])
       (define bstr (vector-ref row 1))
       (define path (cond
                     [(equal? "y" (vector-ref row 0))
                      (main-doc-relative->path
                       (cons 'doc (or (hash-ref reader-cache bstr #f)
                                      (let ([v (<-string (open-input-bytes bstr))])
                                        (hash-set! reader-cache bstr v)
                                        v))))]
                     [(bytes? bstr)
                      (bytes->path bstr)]
                     [else ; "placeholder"
                      #f]))
       (unless (or (not path)
                   (hash-ref ok-files path #f))
         (define pathid (vector-ref row 2))
         (query-exec db "DELETE FROM documented WHERE pathid=$1"
                     pathid)
         (query-exec db "DELETE FROM searches WHERE pathid=$1"
                     pathid)
         (query-exec db "DELETE FROM searchSets WHERE pathid=$1"
                     pathid)
         (query-exec db "DELETE FROM dependencies WHERE pathid=$1"
                     pathid)
         (query-exec db "DELETE FROM pathids WHERE pathid=$1"
                     pathid)
         (query-exec db "INSERT INTO pathids VALUES ($1, 'n', 'placeholder')"
                     pathid))))))
         

(define (filename->pathid db filename)
  (define filename* (path->main-doc-relative filename))
  (define filename-bytes (cond
                          [(pair? filename*)
                           (string->bytes/utf-8 (->string (cdr filename*)))]
                          [(path? filename*)
                           (path->bytes filename*)]
                          [else (path->bytes (string->path filename*))]))
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
   [else (vector-ref id 0)]))

(define reader-cache (make-weak-hash))

(define (pathid->filename db pathid in-other? main-doc-relative-ok?)
  (define row (query-maybe-row db 
                               (if in-other?
                                   select-other-path-vq
                                   select-path-vq)
                               pathid))
  (and row
       (let ([path (vector-ref row 1)])
         (if (equal? "y" (vector-ref row 0))
             ((if main-doc-relative-ok? values main-doc-relative->path)
              (cons 'doc (or (hash-ref reader-cache path #f)
                             (let ([v (<-string (open-input-bytes path))])
                               (hash-set! reader-cache path v)
                               v))))
             (bytes->path path)))))

(define (prepare-tables db)
  (when (null? 
         (query-rows db (~a "SELECT name FROM sqlite_master"
                            " WHERE type='table' AND name='documented'")))
    (query-exec db (~a "CREATE TABLE documented "
                       "(stag VARCHAR(256),"
                       " pathid SMALLINT,"
                       " UNIQUE (stag, pathid))"))
    (query-exec db (~a "CREATE INDEX documentedStags "
                       "on documented (stag)"))
    (query-exec db (~a "CREATE TABLE dependencies "
                       "(pathid SMALLINT,"
                       " stag VARCHAR(256))"))
    (query-exec db (~a "CREATE TABLE searchSets "
                       "(pathid SMALLINT, "
                       " setid SMALLINT, "
                       " stag VARCHAR(256),"
                       " PRIMARY KEY (pathid, setid))"))
    (query-exec db (~a "CREATE TABLE searches "
                       "(pathid SMALLINT,"
                       " setid SMALLINT, "
                       " stag VARCHAR(256))"))
    (query-exec db (~a "CREATE TABLE pathids "
                       "(pathid SMALLINT,"
                       " atmain CHAR(1),"
                       " path VARCHAR(1024),"
                       " PRIMARY KEY (pathid))"))
    (query-exec db (~a "CREATE INDEX dependenciesPath "
                       "on dependencies (pathid)"))
    (query-exec db (~a "CREATE INDEX searchSetsPath "
                       "on searchSets (pathid)"))
    (query-exec db (~a "CREATE INDEX searchesTag "
                       "on searches (stag)"))
    (query-exec db (~a "CREATE INDEX searchesPathId "
                       "on searches (pathid, setid)"))))

(define (exn:fail:retry? v)
  (and (exn:fail:sql? v)
       (let ([s (exn:fail:sql-sqlstate v)])
         (or (eq? s 'busy)
             (and (string? s) (regexp-match? s #rx"^40...$"))))))

(define (call-with-lock-handler handler thunk)
  (with-handlers* ([exn:fail:retry?
                    (lambda (exn) (handler))])
    (thunk)))

(define (call-with-retry/transaction who db write? thunk)
  (let loop ([pause (doc-db-init-pause)])
    ((let/ec esc
       (define at-commit? #f)
       (define success? #f)
       (dynamic-wind
        (lambda ()
          (call-with-lock-handler
           (lambda () (esc (lambda ()
                             (loop (doc-db-pause `(start ,who) pause)))))
           (lambda () (start-transaction db #:option (if write? 'immediate #f)))))
        (lambda ()
          (call-with-lock-handler
           (lambda () (esc (lambda ()
                             (loop (doc-db-pause `(,(if at-commit? 'commit 'query)
                                                   ,who)
                                                 pause)))))
           (lambda ()
             (define l (call-with-values thunk list))
             (set! at-commit? #t)
             (commit-transaction db)
             (set! success? #t)
             (lambda () (apply values l)))))
        (lambda ()
          (unless success?
            (rollback db 1))))))))

(define (rollback db count)
  (when (in-transaction? db)
    (with-handlers* ([exn:fail:retry?
                      (lambda (exn)
                        (log-doc-db-info "database locked on rollback; tried ~a times so far"
                                         count)
                        (rollback db (add1 count)))])
      (rollback-transaction db))))

(define (call-with-retry who db thunk)
  (let loop ([pause (doc-db-init-pause)])
    (call-with-lock-handler
     (lambda () (loop (doc-db-pause `(query ,who) pause)))
     thunk)))

(define (readable? s)
  (or (string? s)
      (bytes? s)
      (symbol? s)
      (number? s)
      (boolean? s)
      (and (list? s) (andmap readable? s))
      (and (vector? s) (for/and ([v (in-vector s)]) (readable? s)))))

(define (->string v)
  (if (and (not (box? v))
           (readable? v))
      (~s v)
      (~s (box (serialize v)))))

(define (<-string p)
  (with-handlers ([exn:fail:read?
                   (lambda (exn)
                     (error 'read
                            (~a "error unmarshaling from database\n"
                                "  original error: ~e")
                            (exn-message exn)))])
    (define v (read p))
    (if (box? v)
        ;; A box indicates serialization:
        (deserialize (unbox v))
        ;; Normal value:
        v)))
