#lang racket/base
(require racket/contract/base
         racket/format
         racket/set
         racket/path
         racket/file
         version/utils
         setup/dirs
         db/private/pre)

(provide 
 (struct-out pkg)
 (contract-out
  [current-pkg-catalog-file
   (parameter/c path-string?)]
  
  [call-with-pkgs-transaction ((-> any) . -> . any)]

  [get-catalogs (-> (listof string?))]
  [set-catalogs! ((listof string?) . -> . void?)]

  [set-pkgs! ((string? (listof (or/c pkg? string?)))
              (#:clear-other-checksums? boolean?)
              . ->* . 
              void?)]

  [get-pkgs (()
             (#:name (or/c #f string?)
                     #:catalog (or/c #f string?))
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

  [get-pkg-dependencies (string? string? string?
                            . -> . (listof dep/c))]
  [set-pkg-dependencies! (string? string? string?
                                  (listof dep/c)
                                  . -> . void?)]

  [get-pkg-tags (string? string?
                            . -> . (listof string?))]
  [set-pkg-tags! (string? string? (listof string?)
                          . -> . void?)]

  [get-module-pkgs (module-path? . -> . (listof pkg?))]

  [get-pkgs-without-modules (()
                             (#:catalog string?)
                             . ->* .
                             (listof pkg?))]))

(define platform/c (or/c string? symbol? regexp?))
(define dep/c (or/c string?
                    (list/c string?)
                    (list/c string? string?)
                    (list/c string? '#:version valid-version?)
                    (list/c string? '#:platform platform/c)
                    (list/c string? '#:version valid-version? '#:platform platform/c)
                    (list/c string? '#:platform platform/c '#:version valid-version?)))

(struct pkg (name catalog author source checksum desc)
  #:transparent)

(define (prepare-catalog-table db)
  (prepare-table db
                 "catalog"
                 (~a "(id SMALLINT,"
                     " url TEXT,"
                     " pos SMALLINT)")))

(define (prepare-pkg-table db)
  (prepare-table db
                 "pkg"
                 (~a "(name TEXT,"
                     " catalog SMALLINT,"
                     " author TEXT,"
                     " source TEXT,"
                     " checksum TEXT,"
                     " desc TEXT)")
                 ;; index:
                 "(name, catalog)"))

(define (prepare-tags-table db)
  (prepare-table db
                 "tags"
                 (~a "(pkg TEXT,"
                     " catalog SMALLINT,"
                     " tag TEXT)")
                 ;; index:
                 "(pkg, catalog)"))

(define (prepare-modules-table db)
  (prepare-table db
                 "modules"
                 (~a "(name TEXT,"
                     " pkg TEXT,"
                     " catalog SMALLINT,"
                     " checksum TEXT)")
                 ;; index:
                 "(pkg, catalog, checksum)"))

(define (prepare-dependencies-table db)
  (prepare-table db
                 "dependencies"
                 (~a "(onpkg TEXT,"
                     " onversion TEXT,"
                     " onplatform TEXT,"
                     " pkg TEXT,"
                     " catalog SMALLINT,"
                     " checksum TEXT)")
                 ;; index:
                 "(pkg, catalog, checksum)"))

(define current-pkg-catalog-file
  (make-parameter (build-path
                   (find-system-path 'addon-dir)
                   (get-installation-name)
                   "pkgs"
                   "catalog.sqlite")))

(define-struct catalog-db (connection in-transaction? ready-tables))
(define current-catalog-db (make-parameter #f))

(define (call-with-pkgs-transaction proc)
  (call-with-catalog-db
   (lambda (db)
     (call-with-transaction
      (catalog-db-connection db)
      (lambda ()
        (parameterize ([current-catalog-db
                        (make-catalog-db (catalog-db-connection db)
                                         #t
                                         (make-hash))])
          (proc)))))))

(define (call-with-catalog-db proc)
  (define file (current-pkg-catalog-file))
  (define dir (path-only file))
  (when dir
    (unless (directory-exists? dir)
      (make-directory* dir)))
  (define catalog-db (current-catalog-db))
  (define db #f)
  (dynamic-wind
      (lambda ()
        (set! db (if catalog-db 
                     (catalog-db-connection catalog-db)
                     (sqlite3-connect #:database file
                                      #:mode 'create
                                      #:busy-retry-limit +inf.0))))
      (lambda () (proc (or catalog-db
                      (make-catalog-db db #f (make-hash)))))
      (lambda () 
        (unless catalog-db
          (disconnect db)))))

(define (call-with-catalog-transaction db proc)
  (if (catalog-db-in-transaction? db)
      ;; In one big transaction:
      (proc)
      ;; Not already in one big transaction:
      (call-with-transaction
       (catalog-db-connection db)
       proc)))

(define (get-pkgs #:name [name #f]
                  #:catalog [catalog #f])
  (call-with-catalog-db
   (lambda (db)
     (prepare-catalog-table db)
     (prepare-pkg-table db)
     (for/list ([row (in-list
                      (apply
                       query-rows
                       (catalog-db-connection db)
                       (~a "SELECT K.name, N.url, K.author, K.source, K.checksum, K.desc"
                           " FROM pkg K, catalog N"
                           " WHERE N.id = K.catalog"
                           (if catalog
                               " AND  N.url = $1"
                               "")
                           (if name
                               (~a " AND  K.name = "
                                   (if catalog "$2" "$1"))
                               "")
                           " ORDER BY N.pos")
                       (append
                        (if catalog
                            (list catalog)
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

(define (set-pkg! name catalog author source checksum desc
                  #:clear-other-checksums? [clear-other-checksums? (not (equal? checksum ""))])
  (call-with-catalog-db
   (lambda (db)
     (prepare-catalog-table db)
     (prepare-pkg-table db)
     (prepare-modules-table db)
     (prepare-dependencies-table db)
     (call-with-catalog-transaction
      db
      (lambda ()
        (define catalog-id (url->catalog db catalog))
        (query (catalog-db-connection db)
               (~a "UPDATE pkg"
                   " SET author=$1, source=$2, checksum=$3, desc=$4"
                   " WHERE name=$5"
                   " AND   catalog=$6")
               author source checksum desc
               name catalog-id)
        (when clear-other-checksums?
          (query-exec (catalog-db-connection db)
                      (~a "DELETE FROM modules"
                          " WHERE catalog=$1 AND pkg=$2 AND checksum<>$3")
                      catalog-id
                      name
                      checksum)
          (query-exec (catalog-db-connection db)
                      (~a "DELETE FROM dependencies"
                          " WHERE catalog=$1 AND pkg=$2 AND checksum<>$3")
                      catalog-id
                      name
                      checksum))
        (void))))))

(define (get-pkg-tags name catalog)
  (call-with-catalog-db
   (lambda (db)
     (prepare-catalog-table db)
     (prepare-pkg-table db)
     (prepare-tags-table db)
     (define catalog-id (url->catalog db catalog))
     (query-list (catalog-db-connection db)
                 (~a "SELECT tag FROM tags"
                     " WHERE catalog=$1"
                     "   AND pkg=$2")
                 catalog-id
                 name))))

(define (set-pkg-tags! name catalog tags)
  (call-with-catalog-db
   (lambda (db)
     (prepare-catalog-table db)
     (prepare-pkg-table db)
     (prepare-tags-table db)
     (call-with-catalog-transaction
      db
      (lambda ()
        (define catalog-id (url->catalog db catalog))
        (query-exec (catalog-db-connection db)
                    (~a "DELETE FROM tags"
                        " WHERE catalog=$1"
                        "   AND pkg=$2")
                    catalog-id
                    name)
        (for ([tag (in-list tags)])
         (query (catalog-db-connection db)
                (~a "INSERT INTO tags"
                    " VALUES ($1, $2, $3)")
                name catalog-id tag)))))))

(define (get-pkg-modules name catalog checksum)
  (call-with-catalog-db
   (lambda (db)
     (prepare-catalog-table db)
     (prepare-pkg-table db)
     (prepare-modules-table db)
     (define catalog-id (url->catalog db catalog))
     (map
      string->mod
      (query-list (catalog-db-connection db)
                  (~a "SELECT name FROM modules"
                      " WHERE catalog=$1"
                      "  AND pkg=$2"
                      "  AND checksum=$3")
                  catalog-id
                  name
                  checksum)))))

(define (set-pkg-modules! name catalog checksum modules)
  (call-with-catalog-db
   (lambda (db)
     (prepare-catalog-table db)
     (prepare-pkg-table db)
     (prepare-modules-table db)
     (call-with-catalog-transaction
      db
      (lambda ()
        (define catalog-id (url->catalog db catalog))
        (query-exec (catalog-db-connection db)
                    (~a "DELETE FROM modules"
                        " WHERE catalog=$1"
                        "  AND pkg=$2"
                        "  AND checksum=$3")
                    catalog-id
                    name
                    checksum)
        (for ([mod (in-list modules)])
         (query (catalog-db-connection db)
                (~a "INSERT INTO modules"
                    " VALUES ($1, $2, $3, $4)")
                (mod->string mod) name catalog-id checksum)))))))

(define (get-module-pkgs mod)
  (call-with-catalog-db
   (lambda (db)
     (prepare-catalog-table db)
     (prepare-modules-table db)
     (define rows
       (query-rows (catalog-db-connection db)
                   (~a "SELECT M.pkg, P.url, M.checksum"
                       " FROM modules M, catalog P"
                       " WHERE M.name = $1"
                       "  AND  M.catalog = P.id")
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

(define (get-pkgs-without-modules #:catalog [catalog #f])
  (call-with-catalog-db
   (lambda (db)
     (prepare-catalog-table db)
     (prepare-pkg-table db)
     (prepare-modules-table db)
     (define rows
       (apply
        query-rows
        (catalog-db-connection db)
        (~a "SELECT K.name, N.url, K.checksum"
            " FROM pkg K, catalog N"
            " WHERE N.id = K.catalog"
            (if catalog
                " AND  N.url = $1"
                "")
            " AND NOT EXISTS"
            " (SELECT M.name"
            "  FROM modules M"
            "  WHERE M.pkg = K.name"
            "   AND  M.catalog = K.catalog"
            "   AND  M.checksum = K.checksum)")
        (append
         (if catalog
             (list catalog)
             null))))
     (for/list ([row (in-list rows)])
       (pkg (vector-ref row 0)
            (vector-ref row 1)
            ""
            ""
            (vector-ref row 2)
            "")))))

(define (get-pkg-dependencies name catalog checksum)
  (call-with-catalog-db
   (lambda (db)
     (prepare-catalog-table db)
     (prepare-pkg-table db)
     (prepare-dependencies-table db)
     (define catalog-id (url->catalog db catalog))
     (define rows
       (query-rows (catalog-db-connection db)
                   (~a "SELECT onpkg, onversion, onplatform"
                       " FROM dependencies"
                       " WHERE catalog=$1"
                       "  AND pkg=$2"
                       "  AND checksum=$3")
                   catalog-id
                   name
                   checksum))
     (for/list ([row (in-list rows)])
       (define on-pkg (vector-ref row 0))
       (define on-version (vector-ref row 1))
       (define on-platform (vector-ref row 2))
       (cons on-pkg
             (append
              (if (equal? on-version "")
                  null
                  (list '#:version on-version))
              (if (equal? on-platform "")
                  null
                  (list '#:platform (string->platform on-platform)))))))))

(define (set-pkg-dependencies! name catalog checksum dependencies)
  (define (get-keyed l k wrap)
    (define a (memq k l))
    (if a (wrap (cadr a)) ""))
  (call-with-catalog-db
   (lambda (db)
     (prepare-catalog-table db)
     (prepare-pkg-table db)
     (prepare-dependencies-table db)
     (call-with-catalog-transaction
      db
      (lambda ()
        (define catalog-id (url->catalog db catalog))
        (query-exec (catalog-db-connection db)
                    (~a "DELETE FROM dependencies"
                        " WHERE catalog=$1"
                        "  AND pkg=$2"
                        "  AND checksum=$3")
                    catalog-id
                    name
                    checksum)
        (for ([dep (in-list dependencies)])
         (query (catalog-db-connection db)
                (~a "INSERT INTO dependencies"
                    " VALUES ($1, $2, $3, $4, $5, $6)")
                (cond
                 [(string? dep) dep]
                 [else (car dep)])
                (cond
                 [(string? dep) ""]
                 [(and (list? dep) (= 2 (length dep)))
                  (cadr dep)]
                 [else (get-keyed (cdr dep) '#:version values)])
                (cond
                 [(string? dep) ""]
                 [(and (list? dep) (= 2 (length dep)))
                  ""]
                 [else (get-keyed (cdr dep) '#:platform platform->string)])
                name catalog-id checksum)))))))

(define (platform->string dep) (~s dep))
(define (string->platform str) (read (open-input-string str)))

(define (get-catalogs)
  (call-with-catalog-db
   (lambda (db)
     (prepare-catalog-table db)
     (query-list (catalog-db-connection db)
                 (~a "SELECT url FROM catalog"
                     " ORDER BY pos")))))

(define (set-catalogs! urls)
  (call-with-catalog-db
   (lambda (db)
     (prepare-catalog-table db)
     (prepare-pkg-table db)
     (prepare-tags-table db)
     (prepare-modules-table db)
     (prepare-dependencies-table db)
     (call-with-catalog-transaction
      db
      (lambda ()
        (define current-url+ids
          (query-rows (catalog-db-connection db) "SELECT url, id FROM catalog"))
        (define old-urls (for/list ([old (in-list current-url+ids)])
                           (vector-ref old 0)))
        (for ([old (in-list current-url+ids)])
          (define old-url (vector-ref old 0))
          (define old-id (vector-ref old 1))
          (unless (member old-url urls)
            (query-exec (catalog-db-connection db)
                        "DELETE FROM catalog WHERE id=$1"
                        old-id)
            (query-exec (catalog-db-connection db)
                        "DELETE FROM pkg WHERE catalog=$1"
                        old-id)
            (query-exec (catalog-db-connection db)
                        "DELETE FROM tags WHERE catalog=$1"
                        old-id)
            (query-exec (catalog-db-connection db)
                        "DELETE FROM modules WHERE catalog=$1"
                        old-id)
            (query-exec (catalog-db-connection db)
                        "DELETE FROM dependencies WHERE catalog=$1"
                        old-id)))
        (for ([new-url (in-list urls)])
          (unless (member new-url old-urls)
            (let loop ([id 0])
              (if (query-maybe-row (catalog-db-connection db)
                                   "SELECT url FROM catalog WHERE id=$1"
                                   id)
                  (loop (add1 id))
                  (query-exec (catalog-db-connection db)
                              "INSERT INTO catalog VALUES ($1, $2, 0)"
                              id
                              new-url)))))
        (for ([new-url (in-list urls)]
              [pos (in-naturals)])
          (query-exec (catalog-db-connection db)
                      (~a "UPDATE catalog"
                          " SET pos = $1"
                          " WHERE url = $2")
                      pos
                      new-url)))))))

(define (url->catalog db url)
  (query-value (catalog-db-connection db)
               "SELECT id FROM catalog WHERE url=$1"
               url))

(define (set-pkgs! url pkgs
                   #:clear-other-checksums? [clear-other-checksums? #t])
  (call-with-catalog-db
   (lambda (db)
     (prepare-catalog-table db)
     (prepare-pkg-table db)
     (prepare-modules-table db)
     (prepare-dependencies-table db)
     (call-with-catalog-transaction
      db
      (lambda ()
        (define catalog (url->catalog db url))
        (define pkg-names (for/list ([p (in-list pkgs)])
                            (if (pkg? p)
                                (pkg-name p)
                                p)))
        (define current-pkgs
          (query-list (catalog-db-connection db)
                      "SELECT name FROM pkg WHERE catalog=$1"
                      catalog))
        (define new-pkgs (list->set pkg-names))
        (define old-pkgs (list->set current-pkgs))
        (for ([old (in-list current-pkgs)])
          (unless (set-member? new-pkgs old)
            (query-exec (catalog-db-connection db)
                        "DELETE FROM pkg WHERE catalog=$1 AND name=$2"
                        catalog
                        old)
            (query-exec (catalog-db-connection db)
                        "DELETE FROM tags WHERE catalog=$1 AND pkg=$2"
                        catalog
                        old)
            (query-exec (catalog-db-connection db)
                        "DELETE FROM modules WHERE catalog=$1 AND pkg=$2"
                        catalog
                        old)
            (query-exec (catalog-db-connection db)
                        "DELETE FROM dependencies WHERE catalog=$1 AND pkg=$2"
                        catalog
                        old)))
        (for ([new0 (in-list pkgs)])
          (define new (if (pkg? new0) 
                          new0
                          (pkg new0 "" "" "" "" "")))
          (when (and clear-other-checksums?
                     (not (equal? "" (pkg-checksum new))))
            (query-exec (catalog-db-connection db)
                        (~a "DELETE FROM modules"
                            " WHERE catalog=$1 AND pkg=$2 AND checksum<>$3")
                        catalog
                        (pkg-name new)
                        (pkg-checksum new))
            (query-exec (catalog-db-connection db)
                        (~a "DELETE FROM dependencies"
                            " WHERE catalog=$1 AND pkg=$2 AND checksum<>$3")
                        catalog
                        (pkg-name new)
                        (pkg-checksum new)))
          (unless (and (string? new0)
                       (set-member? old-pkgs new0))
            (if (set-member? old-pkgs (pkg-name new))
                (query-exec (catalog-db-connection db)
                            (~a "UPDATE pkg"
                                " SET author=$1, source=$2, checksum=$3, desc=$4"
                                " WHERE name=$5"
                                " AND   catalog=$6")
                            (pkg-author new)
                            (pkg-source new)
                            (pkg-checksum new)
                            (pkg-desc new)
                            (pkg-name new)
                            catalog)
                (query-exec (catalog-db-connection db)
                            "INSERT INTO pkg VALUES ($1, $2, $3, $4, $5, $6)"
                            (pkg-name new)
                            catalog
                            (pkg-author new)
                            (pkg-source new)
                            (pkg-checksum new)
                            (pkg-desc new))))))))))

(define (prepare-table db which desc [index #f])
  (define ready-tables (catalog-db-ready-tables db))
  (unless (hash-ref ready-tables which #f)
    (hash-set! ready-tables which #t)
    (when (null? 
           (query-rows (catalog-db-connection db)
                       (~a "SELECT name FROM sqlite_master"
                           " WHERE type='table' AND name='" which "'")))
      (query-exec (catalog-db-connection db)
                  (~a "CREATE TABLE " which " "
                      desc))
      (when index
        (query-exec (catalog-db-connection db)
                    (~a "CREATE INDEX " which "_index "
                        "ON " which " " index))))))
