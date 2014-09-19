#lang racket/base
(require pkg/lib
         (prefix-in db: pkg/db)
         racket/file
         racket/format
         racket/string
         "shelly.rkt"
         "util.rkt")

(this-test-is-run-by-the-main-test)

(pkg-tests
 (shelly-begin
  (initialize-catalogs)

  $ "raco pkg config --set catalogs http://localhost:9990"

  $ "racket -l racket/base -l pkg/lib -e '(pkg-config-catalogs)'"
  =stdout> "'(\"http://localhost:9990\")\n"

  $ "racket -l racket/base -l tests/pkg/test-catalogs-api -e '(test-api)'"
  =stderr> ""

  (define d (make-temporary-file "pkg-~a" 'directory))
  (define db (build-path d "catalog.sqlite"))
  (define dir (build-path d "catalog"))
  (define dir2 (build-path d "catalog2"))
  $ (~a "raco pkg catalog-copy --from-config " (path->string db))
  $ (~a "raco pkg config --set catalogs file://" (path->string db))

  $ "raco pkg catalog-show pkg-test1"
  =stdout> #rx"Source: http://localhost:9999/pkg-test1.zip"

  (parameterize ([db:current-pkg-catalog-file db])
    (db:set-pkgs! "local"
                  (append (db:get-pkgs)
                          (list
                           (db:pkg "fish" "local" "nemo@sub" "http://localhost:9999/fish.zip" "123" 
                                   "Not a whale"))))
    (db:set-pkg-modules! "fish" "local" "123" '((lib "fish/main.rkt") (lib "fish/food.rkt")))
    (db:set-pkg-dependencies! "fish" "local" "123"
                              '("ocean" ("water" "1.0") ("crash-helmet" #:platform windows))))

  $ "raco pkg catalog-show fish" =stdout> #rx"Checksum: 123"
  $ "raco pkg catalog-show fish" =stdout> #rx"ocean"
  $ "raco pkg catalog-show fish" =stdout> #rx"water version 1.0"
  $ "raco pkg catalog-show fish" =stdout> #rx"crash-helmet on platform 'windows"
  $ "raco pkg catalog-show --modules fish" =stdout> #rx"fish/food"

  $ (~a "raco pkg catalog-copy " (path->string db) " " (path->string dir))
  $ (~a "raco pkg config --set catalogs file://" (path->string dir))
  $ "raco pkg catalog-show fish" =stdout> #rx"Checksum: 123"
  $ "raco pkg catalog-show --only-names fish" =stdout> #rx"fish"
  $ "raco pkg catalog-show --only-names --all" =stdout> #rx"fish"
  $ "raco pkg catalog-show --modules fish" =stdout> #rx"fish/food"
  $ "raco pkg catalog-show fish" =stdout> #rx"water version 1.0"
 
  (delete-file (build-path dir "pkgs"))
  (delete-file (build-path dir "pkgs-all"))
  $ "raco pkg catalog-show fish" =stdout> #rx"Checksum: 123"
  $ "raco pkg catalog-show --only-names fish" =stdout> #rx"^fish"
  $ "raco pkg catalog-show --only-names --all" =stdout> #rx"^fish"
  $ "raco pkg catalog-show --modules fish" =stdout> #rx"fish/food"
  $ "raco pkg catalog-show fish" =stdout> #rx"water version 1.0"

  (delete-file (build-path dir "pkg/fish"))
  $ "raco pkg catalog-show fish" =exit> 1
  
  (define (try-merge dest)
    (shelly-begin
     $ (~a "raco pkg config --set catalogs file://" (path->string dest))
     
     (make-directory* (build-path dir2 "pkg"))
     (define (add-whale! cksum)
       (call-with-output-file* 
        (build-path dir2 "pkg" "whale")
        #:exists 'truncate
        (lambda (o)
          (write (hash 'name "whale"
                       'checksum cksum
                       'source "http://localhost:9999/whale.plt"
                       'versions (hash "5.3.6"
                                       (hash 'checksum
                                             123)))
                 o))))
     (add-whale! "345")
     $ (~a "raco pkg catalog-show --catalog file://" (path->string dir2) " whale")
     =stdout> #rx"Checksum: 345"
     $ (~a "raco pkg catalog-show --version 5.3.6 --catalog file://" (path->string dir2) " whale")
     =stdout> #rx"Checksum: 123"
     $ "raco pkg catalog-show whale" =exit> 1
     
     $ (~a "raco pkg catalog-copy --merge " (path->string dir2) " " (path->string dest))
     $ "raco pkg catalog-show whale" =stdout> #rx"Checksum: 345"
     
     (add-whale! "567")
     $ (~a "raco pkg catalog-copy --merge " (path->string dir2) " " (path->string dest))
     $ "raco pkg catalog-show whale" =stdout> #rx"Checksum: 345"
     $ (~a "raco pkg catalog-copy --merge --override " (path->string dir2) " " (path->string dest))
     $ "raco pkg catalog-show whale" =stdout> #rx"Checksum: 567"))

  (try-merge dir)
  (try-merge db)

  ;; catalog-archive:

  (define archive-d (build-path d "archive"))
  
  $ (~a "raco pkg catalog-archive " archive-d " http://localhost:9990")
  $ (~a "test -f " archive-d "/pkgs/pkg-test1.zip")

  (define (rx:pkg-test1 as-url?)
    (regexp
     (~a (regexp-quote (~a "Source: " (if as-url? "file://" "") archive-d "/pkgs/pkg-test1.zip"))
         ".*"
         (regexp-quote (~a "Checksum: " (file->string 
                                         (build-path archive-d
                                                     "pkgs"
                                                     "pkg-test1.zip.CHECKSUM")))))))
  
  $ (~a "raco pkg catalog-show --catalog file://" archive-d "/catalog pkg-test1")
  =stdout> (rx:pkg-test1 #f)

  (delete-directory/files archive-d)

  $ "raco pkg config --set catalogs http://localhost:9990"

  $ (~a "raco pkg catalog-archive --from-config --relative"
        " --state " (build-path archive-d "state.sqlite")
        " " archive-d)
  =stdout> #rx"== Archiving pkg-test1 =="
  $ (~a "raco pkg catalog-show --catalog file://" archive-d "/catalog pkg-test1")
  =stdout> (rx:pkg-test1 #t)
  $ (~a "grep archive " archive-d "/catalog/pkg/pkg-test1") ; relative path => no "archive"
  =exit> 1
  $ (~a "test -f " archive-d "/pkgs/pkg-test2.zip")
  $ (~a "test -f " archive-d "/pkgs/pkg-test2-snd.zip") =exit> 1
  $ (~a "raco pkg catalog-show --catalog file://" archive-d "/catalog pkg-test2")
  =stdout> #px"Dependencies:\\s+ pkg-test1"
  $ (~a "raco pkg catalog-show --catalog file://" archive-d "/catalog pkg-test1")
  =stdout> #px"Tags: first"

  ;; Incremental update:
  $ (~a "raco pkg catalog-archive --from-config --relative"
        " --state " (build-path archive-d "state.sqlite")
        " " archive-d
        " http://localhost:9991")
  =stdout> #rx"== Archiving pkg-test2-snd =="
  $ (~a "test -f " archive-d "/pkgs/pkg-test2.zip")
  $ (~a "test -f " archive-d "/pkgs/pkg-test2-snd.zip")
  $ (~a "test -f " archive-d "/pkgs/pkg-test2-snd.zip.CHECKSUM")
  
  ;; Delete package not in source archives:
  $ (~a "raco pkg catalog-archive --from-config --relative"
        " --state " (build-path archive-d "state.sqlite")
        " " archive-d)
  $ (~a "test -f " archive-d "/pkgs/pkg-test2-snd.zip") =exit> 1
  $ (~a "test -f " archive-d "/pkgs/pkg-test2-snd.zip.CHECKSUM") =exit> 1
  
  ;; archive

  (delete-directory/files archive-d)

  $ (~a "raco pkg install pkg-test1")
  $ (~a "raco pkg archive " archive-d " pkg-test1")
  =stdout> #rx"== Archiving pkg-test1 =="
  $ (~a "test -f " archive-d "/pkgs/pkg-test1.zip")
  $ (~a "test -f " archive-d "/pkgs/pkg-test1.zip.CHECKSUM")


  $ "raco pkg install pkg-test2"
  $ (~a "raco pkg archive " archive-d " pkg-test2")
  =stdout> #rx"Removing .* pkg-test1"
  $ (~a "test -f " archive-d "/pkgs/pkg-test2.zip")
  $ (~a "test -f " archive-d "/pkgs/pkg-test2.zip.CHECKSUM")

  (delete-directory/files archive-d)

  $ (~a "raco pkg archive --include-deps " archive-d " pkg-test2")
  =stdout> #rx"Archiving pkg-test1" ;; checking dependencies
  $ (~a "test -f " archive-d "/pkgs/pkg-test1.zip")
  $ (~a "test -f " archive-d "/pkgs/pkg-test1.zip.CHECKSUM")
  $ (~a "test -f " archive-d "/pkgs/pkg-test2.zip")
  $ (~a "test -f " archive-d "/pkgs/pkg-test2.zip.CHECKSUM")

  (delete-directory/files d)))
