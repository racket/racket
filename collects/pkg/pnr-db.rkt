#lang racket/base
(require racket/contract/base
         racket/format
         racket/set
         db)

(provide 
 (struct-out pkg)
 (contract-out
  [current-pkg-index-file
   (parameter/c path-string?)]

  [get-indexes (-> (listof string?))]
  [set-indexes! ((listof string?) . -> . void?)]

  [set-pkgs! ((string? (listof (or/c pkg? string?)))
              (#:clear-other-checksums? boolean?)
              . ->* . 
              void?)]

  [get-pkgs (()
             (#:name (or/c #f string?)
                     #:index (or/c #f string?))
             . ->* .
             (listof pkg?))]
  [set-pkg! ((string? string? string? string? string? string?)
             (#:clear-other-checksums? boolean?)
             . ->* .
             void?)]

  [get-pkg-modules (string? string? string?
                            . -> . (listof module-path?))]
  [set-pkg-modules! (string? string? string?
                             (listof module-path?)
                             . -> . void?)]

  [get-pkg-tags (string? string?
                            . -> . (listof string?))]
  [set-pkg-tags! (string? string? (listof string?)
                          . -> . void?)]

  [get-module-pkgs (module-path? . -> . (listof pkg?))]

  [get-pkgs-without-modules (()
                             (#:index string?)
                             . ->* .
                             (listof pkg?))]))

(struct pkg (name index author source checksum desc)
  #:transparent)

(define (prepare-pnr-table db)
  (prepare-table db
                 "pnr"
                 (~a "(id SMALLINT,"
                     " url TEXT,"
                     " pos SMALLINT)")))

(define (prepare-pkg-table db)
  (prepare-table db
                 "pkg"
                 (~a "(name TEXT,"
                     " pnr SMALLINT,"
                     " author TEXT,"
                     " source TEXT,"
                     " checksum TEXT,"
                     " desc TEXT)")))

(define (prepare-tags-table db)
  (prepare-table db
                 "tags"
                 (~a "(pkg TEXT,"
                     " pnr TEXT,"
                     " tag TEXT)")))

(define (prepare-modules-table db)
  (prepare-table db
                 "modules"
                 (~a "(name TEXT,"
                     " pkg TEXT,"
                     " pnr SMALLINT,"
                     " checksum TEXT)")))

(define current-pkg-index-file
  (make-parameter (build-path
                   (find-system-path 'addon-dir)
                   (version)
                   "pnr.sqlite")))

(define (call-with-pnr-db proc)
  (define db #f)
  (dynamic-wind
      (lambda ()
        (set! db (sqlite3-connect #:database (current-pkg-index-file)
                                  #:mode 'create
                                  #:busy-retry-limit +inf.0)))
      (lambda () (proc db))
      (lambda () 
        (disconnect db))))

(define (get-pkgs #:name [name #f]
                  #:index [index #f])
  (call-with-pnr-db
   (lambda (db)
     (prepare-pnr-table db)
     (prepare-pkg-table db)
     (for/list ([row (in-list
                      (apply
                       query-rows
                       db
                       (~a "SELECT K.name, N.url, K.author, K.source, K.checksum, K.desc"
                           " FROM pkg K, pnr N"
                           " WHERE N.id = K.pnr"
                           (if index
                               " AND  N.url = $1"
                               "")
                           (if name
                               (~a " AND  K.name = "
                                   (if index "$2" "$1"))
                               "")
                           " ORDER BY N.pos")
                       (append
                        (if index
                            (list index)
                            null)
                        (if name
                            (list name)
                            null))))])
       (pkg (vector-ref row 0)
            (vector-ref row 1)
            (vector-ref row 2)
            (vector-ref row 3)
            (vector-ref row 4)
            (vector-ref row 5))))))

(define (set-pkg! name index author source checksum desc
                  #:clear-other-checksums? [clear-other-checksums? (not (equal? checksum ""))])
  (call-with-pnr-db
   (lambda (db)
     (prepare-pnr-table db)
     (prepare-pkg-table db)
     (call-with-transaction
      db
      (lambda ()
        (define pnr (url->pnr db index))
        (query db
               (~a "UPDATE pkg"
                   " SET author=$1, source=$2, checksum=$3, desc=$4"
                   " WHERE name=$5"
                   " AND   pnr=$6")
               author source checksum desc
               name pnr)
        (when clear-other-checksums?
          (query-exec db
                      (~a "DELETE FROM modules"
                          " WHERE pnr=$1 AND pkg=$2 AND checksum<>$3")
                      pnr
                      name
                      checksum))
        (void))))))

(define (get-pkg-tags name index)
  (call-with-pnr-db
   (lambda (db)
     (prepare-pnr-table db)
     (prepare-pkg-table db)
     (prepare-tags-table db)
     (define pnr (url->pnr db index))
     (query-list db
                 (~a "SELECT tag FROM tags"
                     " WHERE pnr=$1"
                     "   AND pkg=$2")
                 pnr
                 name))))

(define (set-pkg-tags! name index tags)
  (call-with-pnr-db
   (lambda (db)
     (prepare-pnr-table db)
     (prepare-pkg-table db)
     (prepare-tags-table db)
     (call-with-transaction
      db
      (lambda ()
        (define pnr (url->pnr db index))
        (query-exec db
                    (~a "DELETE FROM tags"
                        " WHERE pnr=$1"
                        "   AND pkg=$2")
                    pnr
                    name)
        (for ([tag (in-list tags)])
         (query db
                (~a "INSERT INTO tags"
                    " VALUES ($1, $2, $3)")
                name pnr tag)))))))

(define (get-pkg-modules name index checksum)
  (call-with-pnr-db
   (lambda (db)
     (prepare-pnr-table db)
     (prepare-pkg-table db)
     (prepare-modules-table db)
     (define pnr (url->pnr db index))
     (map
      string->mod
      (query-list db
                  (~a "SELECT name FROM modules"
                      " WHERE pnr=$1"
                      "  AND pkg=$2"
                      "  AND checksum=$3")
                  pnr
                  name
                  checksum)))))

(define (set-pkg-modules! name index checksum modules)
  (call-with-pnr-db
   (lambda (db)
     (prepare-pnr-table db)
     (prepare-pkg-table db)
     (prepare-modules-table db)
     (call-with-transaction
      db
      (lambda ()
        (define pnr (url->pnr db index))
        (query-exec db
                    (~a "DELETE FROM modules"
                        " WHERE pnr=$1"
                        "  AND pkg=$2"
                        "  AND checksum=$3")
                    pnr
                    name
                    checksum)
        (for ([mod (in-list modules)])
         (query db
                (~a "INSERT INTO modules"
                    " VALUES ($1, $2, $3, $4)")
                (mod->string mod) name pnr checksum)))))))

(define (get-module-pkgs mod)
  (call-with-pnr-db
   (lambda (db)
     (define rows
       (query-rows db
                   (~a "SELECT M.pkg, P.url, M.checksum"
                       " FROM modules M, pnr P"
                       " WHERE M.name = $1"
                       "  AND  M.pnr = P.id")
                   (mod->string mod)))
     (for/list ([row (in-list rows)])
       (pkg (vector-ref row 0)
            (vector-ref row 1)
            ""
            ""
            (vector-ref row 2)
            "")))))

(define (mod->string mp) (~s mp))
(define (string->mod str) (read (open-input-string str)))

(define (get-pkgs-without-modules #:index [index #f])
  (call-with-pnr-db
   (lambda (db)
     (prepare-pnr-table db)
     (prepare-pkg-table db)
     (prepare-modules-table db)
     (define rows
       (apply
        query-rows
        db
        (~a "SELECT K.name, N.url, K.checksum"
            " FROM pkg K, pnr N"
            " WHERE N.id = K.pnr"
            (if index
                " AND  N.url = $1"
                "")
            " AND NOT EXISTS"
            " (SELECT M.name"
            "  FROM modules M"
            "  WHERE M.pkg = K.name"
            "   AND  M.pnr = K.pnr"
            "   AND  M.checksum = K.checksum)")
        (append
         (if index
             (list index)
             null))))
     (for/list ([row (in-list rows)])
       (pkg (vector-ref row 0)
            (vector-ref row 1)
            ""
            ""
            (vector-ref row 2)
            "")))))

(define (get-indexes)
  (call-with-pnr-db
   (lambda (db)
     (prepare-pnr-table db)
     (query-list db (~a "SELECT url FROM pnr"
                        " ORDER BY pos")))))

(define (set-indexes! urls)
  (call-with-pnr-db
   (lambda (db)
     (prepare-pnr-table db)
     (prepare-pkg-table db)
     (prepare-tags-table db)
     (prepare-modules-table db)
     (call-with-transaction
      db
      (lambda ()
        (define current-url+ids
          (query-rows db "SELECT url, id FROM pnr"))
        (define old-urls (for/list ([old (in-list current-url+ids)])
                           (vector-ref old 0)))
        (for ([old (in-list current-url+ids)])
          (define old-url (vector-ref old 0))
          (define old-id (vector-ref old 1))
          (unless (member old-url urls)
            (query-exec db
                        "DELETE FROM pnr WHERE id=$1"
                        old-id)
            (query-exec db
                        "DELETE FROM pkg WHERE pnr=$1"
                        old-id)
            (query-exec db
                        "DELETE FROM tags WHERE pnr=$1"
                        old-id)
            (query-exec db
                        "DELETE FROM modules WHERE pnr=$1"
                        old-id)))
        (for ([new-url (in-list urls)])
          (unless (member new-url old-urls)
            (let loop ([id 0])
              (if (query-maybe-row db
                                   "SELECT url FROM pnr WHERE id=$1"
                                   id)
                  (loop (add1 id))
                  (query-exec db "INSERT INTO pnr VALUES ($1, $2, 0)"
                              id
                              new-url)))))
        (for ([new-url (in-list urls)]
              [pos (in-naturals)])
          (query-exec db (~a "UPDATE pnr"
                             " SET pos = $1"
                             " WHERE url = $2")
                      pos
                      new-url)))))))

(define (url->pnr db url)
  (query-value db
               "SELECT id FROM pnr WHERE url=$1"
               url))

(define (set-pkgs! url pkgs
                   #:clear-other-checksums? [clear-other-checksums? #t])
  (call-with-pnr-db
   (lambda (db)
     (prepare-pnr-table db)
     (prepare-pkg-table db)
     (prepare-modules-table db)
     (call-with-transaction
      db
      (lambda ()
        (define pnr (url->pnr db url))
        (define pkg-names (for/list ([p (in-list pkgs)])
                            (if (pkg? p)
                                (pkg-name p)
                                p)))
        (define current-pkgs
          (query-list db "SELECT name FROM pkg WHERE pnr=$1"
                      pnr))
        (define new-pkgs (list->set pkg-names))
        (define old-pkgs (list->set current-pkgs))
        (for ([old (in-list current-pkgs)])
          (unless (set-member? new-pkgs old)
            (query-exec db
                        "DELETE FROM pkg WHERE pnr=$1 AND name=$2"
                        pnr
                        old)
            (query-exec db
                        "DELETE FROM tags WHERE pnr=$1 AND pkg=$2"
                        pnr
                        old)
            (query-exec db
                        "DELETE FROM modules WHERE pnr=$1 AND pkg=$2"
                        pnr
                        old)))
        (for ([new0 (in-list pkgs)])
          (define new (if (pkg? new0) 
                          new0
                          (pkg new0 "" "" "" "" "")))
          (when (and clear-other-checksums?
                     (not (equal? "" (pkg-checksum new))))
            (query-exec db
                        (~a "DELETE FROM modules"
                            " WHERE pnr=$1 AND pkg=$2 AND checksum<>$3")
                        pnr
                        (pkg-name new)
                        (pkg-checksum new)))
          (unless (and (string? new0)
                       (set-member? old-pkgs new0))
            (if (set-member? old-pkgs (pkg-name new))
                (query-exec db
                            (~a "UPDATE pkg"
                                " SET author=$1, source=$2, checksum=$3, desc=$4"
                                " WHERE name=$5"
                                " AND   pnr=$6")
                            (pkg-author new)
                            (pkg-source new)
                            (pkg-checksum new)
                            (pkg-desc new)
                            (pkg-name new)
                            pnr)
                (query-exec db
                            "INSERT INTO pkg VALUES ($1, $2, $3, $4, $5, $6)"
                            (pkg-name new)
                            pnr
                            (pkg-author new)
                            (pkg-source new)
                            (pkg-checksum new)
                            (pkg-desc new))))))))))

(define (prepare-table db which desc)
  (when (null? 
         (query-rows db (~a "SELECT name FROM sqlite_master"
                            " WHERE type='table' AND name='" which "'")))
    (query-exec db (~a "CREATE TABLE " which " "
                       desc))))

;; ----------------------------------------

(module+ main
  (require rackunit
           racket/file)

  (define (pkg<? a b)
    (if (string=? (pkg-name a) (pkg-name b))
        (string<? (pkg-index a) (pkg-index b))
        (string<? (pkg-name a) (pkg-name b))))

  (parameterize ([current-pkg-index-file (make-temporary-file
                                          "~a.sqlite")])
    (check-equal? (get-indexes) '())

    (set-indexes! '("http://a" "http://b"))
    (check-equal? (get-indexes)
                  '("http://a" "http://b"))
    
    (check-equal? (get-pkgs) '())

    
    (set-pkgs! "http://a" '("p1"))
    (check-equal? (get-pkgs) 
                  (list
                   (pkg "p1" "http://a" "" "" "" "")))
    
    (set-pkgs! "http://b" '("p2"))
    (check-equal? (get-pkgs)
                  (list
                   (pkg "p1" "http://a" "" "" "" "")
                   (pkg "p2" "http://b" "" "" "" "")))
    (check-equal? (get-pkgs #:index "http://a") 
                  (list
                   (pkg "p1" "http://a" "" "" "" "")))
    (check-equal? (get-pkgs #:name "p1")
                  (list
                   (pkg "p1" "http://a" "" "" "" "")))
    
    (set-pkg! "p1" "http://a" "github:a" "adam" "123" "the first package")
    (check-equal? (get-pkgs)
                  (list
                   (pkg "p1" "http://a" "github:a" "adam" "123" "the first package")
                   (pkg "p2" "http://b" "" "" "" "")))

    ;; reverse order of indexes:
    (set-indexes! '("http://b" "http://a"))
    (check-equal? (get-indexes)
                  '("http://b" "http://a"))
    (check-equal? (get-pkgs)
                  (list
                   (pkg "p2" "http://b" "" "" "" "")
                   (pkg "p1" "http://a" "github:a" "adam" "123" "the first package")))

    (check-equal? (get-pkg-tags "p2" "http://b")
                  '())
    (set-pkg-tags! "p2" "http://b" '("2x" "2y" "2z"))
    (check-equal? (sort (get-pkg-tags "p2" "http://b") string<?)
                  '("2x" "2y" "2z"))
    (check-equal? (get-pkg-tags "p1" "http://a")
                  '())
    
    (set-pkg-modules! "p1" "http://a" "123" (list '(lib "lib1/main.rkt")
                                                  '(lib "lib2/main.rkt")))
    (check-equal? (sort (get-pkg-modules "p1" "http://a" "123")
                        string<?
                        #:key cadr)
                  (list '(lib "lib1/main.rkt")
                        '(lib "lib2/main.rkt")))
    (check-equal? (get-module-pkgs '(lib "lib1/main.rkt"))
                  (list
                   (pkg "p1" "http://a" "" "" "123" "")))

    (set-indexes! '("http://a" "http://c"))
    (check-equal? (sort (get-indexes) string<?) 
                  '("http://a" "http://c"))

    (check-equal? (get-pkgs) 
                  (list
                   (pkg "p1" "http://a" "github:a" "adam" "123" "the first package")))

    (delete-file (current-pkg-index-file))

    (void)))
