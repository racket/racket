#lang racket/base
(require rackunit
         racket/file
         racket/format
         pkg/util
         (prefix-in db: pkg/db)
         "shelly.rkt"
         "util.rkt")

(this-test-is-run-by-the-main-test)

(pkg-tests
 (shelly-begin
  (define pkgs-dir (make-temporary-file "~a-pkgs" 'directory))
  (define db (build-path pkgs-dir "catalog.sqlite"))
  (define pkg-x-dir (build-path pkgs-dir "pkg-x"))

  (make-directory* pkg-x-dir)
  (call-with-output-file*
   (build-path pkg-x-dir "info.rkt")
   (lambda (o) 
     (displayln "#lang info" o)
     (write `(define deps '(("pkg-x-windows" #:platform windows)
                            ("pkg-x-unix" #:platform unix)
                            ("pkg-x-macosx" #:platform macosx)
                            ("pkg-x-platform1" 
                             #:platform 
                             ,(path->string (system-library-subpath #f)))
                            ("pkg-x-platform2" #:platform #rx".")
                            ("pkg-x-platform-no" #:platform #rx"no such platform")))
            o)))

  (parameterize ([db:current-pkg-catalog-file db])
    (db:set-catalogs! '("local"))
    (db:set-pkgs! "local"
                  '("pkg-x" "pkg-x-windows" "pkg-x-unix" "pkg-x-macosx"
                    "pkg-x-platform1" "pkg-x-platform2")))
  
  (define (create-package name)
    (define coll-dir (build-path pkgs-dir name))
    (make-directory* coll-dir)
    (call-with-output-file*
     (build-path coll-dir "main.rkt")
     (lambda (o) 
       (displayln "#lang racket/base" o)))
    (parameterize ([db:current-pkg-catalog-file db])
      (db:set-pkg! name "local" "author@place" (path->string coll-dir) "123456" "")))

  (create-package "pkg-x")
  (create-package "pkg-x-unix")
  (create-package "pkg-x-windows")
  (create-package "pkg-x-macosx")
  (create-package "pkg-x-platform1")
  (create-package "pkg-x-platform2")

  (with-fake-root
   (shelly-begin
    $ (~a "raco pkg config --set catalogs file://" (path->string db))
    $ "racket -e '(require pkg-x)'" =exit> 1
    $ "raco pkg install --deps search-auto pkg-x" =exit> 0
    $ "racket -e '(require pkg-x)'" =exit> 0
    $ (~a "racket -e '(require pkg-x-" (system-type) ")'") =exit> 0
    $ "racket -e '(require pkg-x-platform1)'" =exit> 0
    $ "racket -e '(require pkg-x-platform2)'" =exit> 0
    $ "racket -e '(require pkg-x-platform-no)'" =exit> 1
    $ "raco pkg remove pkg-x"))

  (delete-directory/files pkgs-dir)))


