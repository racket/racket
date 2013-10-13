#lang racket/base
(require rackunit
         racket/system
         unstable/debug
         racket/match
         (for-syntax racket/base
                     syntax/parse)
         racket/file
         racket/runtime-path
         racket/path
         racket/list
         pkg/util
         "shelly.rkt"
         "util.rkt")

(pkg-tests
 (shelly-begin
  (shelly-case
   "create"

   (shelly-case
    "create fails on missing directories"
    $ "rm -fr test-pkgs/does-not-exist test-pkgs/does-not-exist.tgz"
    $ "raco pkg create --format tgz test-pkgs/does-not-exist" =exit> 1
    $ "test -f test-pkgs/does-not-exist.tgz" =exit> 1)

   $ "raco pkg create pkg-test1" =exit> 1

   (define-syntax-rule (shelly-create pkg fmt)
     (shelly-case
      (format "create format ~a" fmt)
      $ (format "rm -f test-pkgs/~a.~a test-pkgs/~a.~a.CHECKSUM"
                pkg fmt pkg fmt)
      $ (format "raco pkg create --format ~a test-pkgs/~a"
                fmt pkg)
      $ (format "test -f test-pkgs/~a.~a" pkg fmt)
      $ (format "test -f test-pkgs/~a.~a.CHECKSUM" pkg fmt)))

   (shelly-create "pkg-test1" "tgz")
   (shelly-create "pkg-test1" "zip")
   (shelly-create "pkg-test1-v2" "zip")
   (shelly-create "pkg-test1-conflict" "zip")
   (shelly-create "pkg-test1" "plt")
   (shelly-create "racket-conflict" "tgz")
   (shelly-create "pkg-test3" "zip")
   (shelly-create "pkg-test3-v2" "zip")
   (shelly-create "pkg-test3-v3" "zip")
   (shelly-create "pkg-b-second" "plt")
   (shelly-create "pkg-a-first" "plt")

   $ "raco pkg create --format txt test-pkgs/pkg-test1" =exit> 1

   (when (directory-exists? "test-pkgs/pkg-test1b")
     (delete-directory/files "test-pkgs/pkg-test1b"))
   (copy-directory/files "test-pkgs/pkg-test1" "test-pkgs/pkg-test1b")
   (parameterize ([current-directory "test-pkgs/pkg-test1b/pkg-test1"])
     (shelly-begin
      $ "raco make conflict.rkt main.rkt update.rkt"
      $ "rm conflict.rkt main.rkt update.rkt"))
   (shelly-create "pkg-test1b" "zip")

   (shelly-create "pkg-test2" "zip")

   (shelly-case
    "create is robust against ending /s"
    $ "rm -f test-pkgs/pkg-test1.tgz test-pkgs/pkg-test1.tgz.CHECKSUM"
    $ "raco pkg create --format tgz test-pkgs/pkg-test1/"
    $ "test -f test-pkgs/pkg-test1.tgz"
    $ "test -f test-pkgs/pkg-test1.tgz.CHECKSUM"))

  (shelly-case
   "create MANIFESTs"
   $ "rm -f test-pkgs/pkg-test1/MANIFEST"
   $ "raco pkg create --manifest test-pkgs/pkg-test1/"
   $ "test -f test-pkgs/pkg-test1/MANIFEST")))
