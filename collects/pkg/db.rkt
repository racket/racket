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

  [get-pnr-urls (-> (listof string?))]
  [set-pnr-urls! ((listof string?) . -> . void?)]

  [set-pnr-pkgs! (string? (listof string?) . -> . void?)]

  [get-pkgs (()
             (#:pnr-url (or/c #f string?)
                        #:name (or/c #f string?))
             . ->* .
             (listof pkg?))]
  [set-pkg! (string? string? string? string? string? string?
                     . -> .
                     void?)]

  [get-pkg-modules (string? string? string?
                            . -> . (listof module-path?))]
  [set-pkg-modules! (string? string? string?
                             (listof module-path?)
                             . -> . void?)]))

(struct pkg (name pnr-url author checksum desc tags)
  #:transparent)

(define current-pkg-index-file
  (make-parameter (build-path
                   (find-system-path 'addon-dir)
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

(define (get-pkgs #:pnr-url [pnr-url #f])
  (call-with-pnr-db
   (lambda (db)
     (prepare-pnr-table db)
     (prepare-pkg-table db)
     (for/list ([row (in-list
                      (apply
                       query-rows
                       db
                       (~a "SELECT K.name, N.url, K.author, K.checksum, K.desc, k.tags"
                           " FROM pkg K, pnr N"
                           " WHERE N.id = K.pnr"
                           (if pnr-url
                               " AND  N.url = $1"
                               ""))
                       (if pnr-url
                           (list pnr-url)
                           null)))])
       (pkg (vector-ref row 0)
            (vector-ref row 1)
            (vector-ref row 2)
            (vector-ref row 3)
            (vector-ref row 4)
            (vector-ref row 5))))))

(define (set-pkg! name pnr-url author checksum desc tags)
  (call-with-pnr-db
   (lambda (db)
     (prepare-pnr-table db)
     (prepare-pkg-table db)
     (call-with-transaction
      db
      (lambda ()
        (define pnr (url->pnr db pnr-url))
        (query db
               (~a "UPDATE pkg"
                   " SET author=$1, checksum=$2, desc=$3, tags=$4"
                   " WHERE name=$5"
                   " AND   pnr=$6")
               author checksum desc tags
               name pnr)
        (void))))))

(define (get-pkg-modules name pnr-url checksum)
  (call-with-pnr-db
   (lambda (db)
     (prepare-pnr-table db)
     (prepare-pkg-table db)
     (prepare-modules-table db)
     (define pnr (url->pnr db pnr-url))
     (map
      string->mod
      (query-list db
                  "SELECT name FROM modules WHERE pnr=$1, pkg=$2, checksum=$3"
                  pnr
                  name
                  checksum)))))

(define (set-pkg-modules! name pnr-url checksum modules)
  (call-with-pnr-db
   (lambda (db)
     (prepare-pnr-table db)
     (prepare-pkg-table db)
     (prepare-modules-table db)
     (call-with-transaction
      db
      (lambda ()
        (define pnr (url->pnr db pnr-url))
        (query-exec db
                    "DELETE FROM modules WHERE pnr=$1, pkg=$2, checksum=$3"
                    pnr
                    name
                    checksum)
        (for ([mod (in-list modules)])
         (query db
                (~a "INSERT INTO modules"
                    " VALUES ($1, $2, $3, $4)")
                (mod->string mod) name pnr checksum)))))))

(define (string->mod mp) (~s mp))
(define (mod->string str) (read (open-input-string str)))

(define (get-pnr-urls)
  (call-with-pnr-db
   (lambda (db)
     (prepare-pnr-table db)
     (query-list db (~a "SELECT url FROM pnr")))))

(define (set-pnr-urls! urls)
  (call-with-pnr-db
   (lambda (db)
     (prepare-pnr-table db)
     (prepare-pkg-table db)
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
                        "DELETE FROM modules WHERE pnr=$1"
                        old-id)))
        (for ([new-url (in-list urls)])
          (unless (member new-url old-urls)
            (let loop ([id 0])
              (if (query-maybe-row db
                                   "SELECT url FROM pnr WHERE id=$1"
                                   id)
                  (loop (add1 id))
                  (query-exec db "INSERT INTO pnr VALUES ($1, $2)"
                              id
                              new-url))))))))))

(define (url->pnr db url)
  (query-value db
               "SELECT id FROM pnr WHERE url=$1"
               url))

(define (set-pnr-pkgs! url pkgs)
  (call-with-pnr-db
   (lambda (db)
     (prepare-pnr-table db)
     (prepare-pkg-table db)
     (prepare-modules-table db)
     (call-with-transaction
      db
      (lambda ()
        (define pnr (url->pnr db url))
        (define current-pkgs
          (query-list db "SELECT name FROM pkg WHERE pnr=$1"
                      pnr))
        (define new-pkgs (list->set pkgs))
        (define old-pkgs (list->set current-pkgs))
        (for ([old (in-list current-pkgs)])
          (unless (set-member? new-pkgs old)
            (query-exec db
                        "DELETE FROM pkg WHERE pnr=$1, name=$2"
                        pnr
                        old)
            (query-exec db
                        "DELETE FROM modules WHERE pnr=$1, pkg=$2"
                        pnr
                        old)))
        (for ([new (in-list pkgs)])
          (unless (set-member? old-pkgs new)
            (query-exec db
                        "INSERT INTO pkg VALUES ($1, $2, $3, $4, $5, $6)"
                        new
                        pnr
                        ""
                        ""
                        ""
                        ""))))))))

(define (prepare-pnr-table db)
  (prepare-table db
                 "pnr"
                 (~a "(id SMALLINT,"
                     " url VARCHAR(1024))")))

(define (prepare-pkg-table db)
  (prepare-table db
                 "pkg"
                 (~a "(name VARCHAR(1024),"
                     " pnr SMALLINT,"
                     " author VARCHAR(256),"
                     " checksum VARCHAR(256),"
                     " desc VARCHAR(4096),"
                     " tags VARCHAR(1024))")))

(define (prepare-modules-table db)
  (prepare-table db
                 "modules"
                 (~a "(name VARCHAR(1024),"
                     " pkg VARCHAR(1024),"
                     " pnr SMALLINT,"
                     " checksum VARCHAR(256))")))

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
        (string<? (pkg-pnr-url a) (pkg-pnr-url b))
        (string<? (pkg-name a) (pkg-name b))))

  (parameterize ([current-pkg-index-file (make-temporary-file
                                          "~a.sqlite")])
    (check-equal? (get-pnr-urls) '())

    (set-pnr-urls! '("http://a" "http://b"))
    (check-equal? (sort (get-pnr-urls) string<?) 
                  '("http://a" "http://b"))
    
    (check-equal? (get-pkgs) '())

    
    (set-pnr-pkgs! "http://a" '("p1"))
    (check-equal? (get-pkgs) 
                  (list
                   (pkg "p1" "http://a" "" "" "" "")))
    
    (set-pnr-pkgs! "http://b" '("p2"))
    (check-equal? (sort (get-pkgs) pkg<?)
                  (list
                   (pkg "p1" "http://a" "" "" "" "")
                   (pkg "p2" "http://b" "" "" "" "")))
    (check-equal? (get-pkgs #:pnr-url "http://a") 
                  (list
                   (pkg "p1" "http://a" "" "" "" "")))
    
    (set-pkg! "p1" "http://a" "adam" "123" "the first package" "good")
    (check-equal? (sort (get-pkgs) pkg<?)
                  (list
                   (pkg "p1" "http://a" "adam" "123" "the first package" "good")
                   (pkg "p2" "http://b" "" "" "" "")))

    (set-pnr-urls! '("http://a" "http://c"))
    (check-equal? (sort (get-pnr-urls) string<?) 
                  '("http://a" "http://c"))

    (check-equal? (get-pkgs) 
                  (list
                   (pkg "p1" "http://a" "adam" "123" "the first package" "good")))

    (delete-file (current-pkg-index-file))

    (void)))
