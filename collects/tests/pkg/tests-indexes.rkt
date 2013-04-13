#lang racket/base
(require pkg/lib
         (prefix-in db: pkg/pnr-db)
         racket/file
         racket/format
         "shelly.rkt"
         "util.rkt")

(pkg-tests
 (shelly-begin
  (initialize-indexes)

  $ "raco pkg config --set indexes http://localhost:9990"

  $ "racket -l racket/base -l pkg/lib -e '(pkg-config-indexes)'"
  =stdout> "'(\"http://localhost:9990\")\n"

  $ "racket -l racket/base -l tests/pkg/test-indexes-api -e '(test-api)'"
  =stderr> ""

  (define d (make-temporary-file "pkg-~a" 'directory))
  (define db (build-path d "pnr.sqlite"))
  (define dir (build-path d "pnr"))
  (define dir2 (build-path d "pnr2"))
  $ (~a "raco pkg index-copy --from-config " (path->string db))
  $ (~a "raco pkg config --set indexes file://" (path->string db))

  $ "raco pkg index-show pkg-test1"
  =stdout> #rx"Source: http://localhost:9999/pkg-test1.zip"

  (parameterize ([db:current-pkg-index-file db])
    (db:set-pkgs! "local"
                  (append (db:get-pkgs)
                          (list
                           (db:pkg "fish" "local" "nemo@sub" "http://localhost:9999/fish.zip" "123" 
                                   "Not a whale")))))
  $ "raco pkg index-show fish" =stdout> #rx"Checksum: 123"

  $ (~a "raco pkg index-copy " (path->string db) " " (path->string dir))
  $ (~a "raco pkg config --set indexes file://" (path->string dir))
  $ "raco pkg index-show fish" =stdout> #rx"Checksum: 123"
  $ "raco pkg index-show --only-names fish" =stdout> #rx"fish"
  $ "raco pkg index-show --only-names --all" =stdout> #rx"fish"
  
  (delete-file (build-path dir "pkgs"))
  (delete-file (build-path dir "pkgs-all"))
  $ "raco pkg index-show fish" =stdout> #rx"Checksum: 123"
  $ "raco pkg index-show --only-names fish" =stdout> #rx"^fish"
  $ "raco pkg index-show --only-names --all" =stdout> #rx"^fish"

  (delete-file (build-path dir "pkg/fish"))
  $ "raco pkg index-show fish" =exit> 1
  
  (define (try-merge dest)
    (shelly-begin
     $ (~a "raco pkg config --set indexes file://" (path->string dest))
     
     (make-directory* (build-path dir2 "pkg"))
     (define (add-whale! cksum)
       (call-with-output-file* 
        (build-path dir2 "pkg" "whale")
        #:exists 'truncate
        (lambda (o)
          (write (hash 'name "whale"
                       'checksum cksum
                       'source "http://localhost:9999/whale.plt")
                 o))))
     (add-whale! "345")
     $ (~a "raco pkg index-show --index file://" (path->string dir2) " whale") =stdout> #rx"Checksum: 345"
     $ "raco pkg index-show whale" =exit> 1
     
     $ (~a "raco pkg index-copy --merge " (path->string dir2) " " (path->string dest))
     $ "raco pkg index-show whale" =stdout> #rx"Checksum: 345"
     
     (add-whale! "567")
     $ (~a "raco pkg index-copy --merge " (path->string dir2) " " (path->string dest))
     $ "raco pkg index-show whale" =stdout> #rx"Checksum: 345"
     $ (~a "raco pkg index-copy --merge --override " (path->string dir2) " " (path->string dest))
     $ "raco pkg index-show whale" =stdout> #rx"Checksum: 567"))

  (try-merge dir)
  (try-merge db)

  $ "raco pkg config --set indexes http://localhost:9990"

  (delete-directory/files d)))
