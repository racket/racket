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
  (initialize-catalogs)

  $ "raco pkg create --format plt test-pkgs/pkg-test1/"
  $ "raco pkg create --format plt test-pkgs/pkg-test1-not-conflict/"
  (shelly-install "only modules are considered for conflicts"
                  "test-pkgs/pkg-test1.plt"
                  $ "raco pkg install test-pkgs/pkg-test1-not-conflict.plt")

  (shelly-case
   "conflicts"
   (shelly-install "double install fails" "test-pkgs/pkg-test1.zip"
                   $ "raco pkg install test-pkgs/pkg-test1.zip" =exit> 1)

   (with-fake-root
    (shelly-case
     "conflicts with racket fail"
     $ "test -f test-pkgs/racket-conflict.tgz"
     $ "raco pkg install test-pkgs/racket-conflict.tgz" =exit> 1))

   (shelly-install "conflicts are caught" "test-pkgs/pkg-test1.zip"
                   $ "test -f test-pkgs/pkg-test1-conflict.zip"
                   $ "raco pkg install test-pkgs/pkg-test1-conflict.zip" =exit> 1)

   (shelly-install "conflicts are caught across sharing modes" "test-pkgs/pkg-test1.zip"
                   $ "test -f test-pkgs/pkg-test1-conflict.zip"
                   $ "raco pkg install -s test-pkgs/pkg-test1-conflict.zip" =exit> 1)

   (shelly-install "conflicts are caught for compiled files" "test-pkgs/pkg-test1.zip"
                   $ "test -f test-pkgs/pkg-test1b.zip"
                   $ "raco pkg install test-pkgs/pkg-test1b.zip" =exit> 1)

   (shelly-install* "conflicts are caught in single-collection" 
                    "test-pkgs/pkg-test1.zip test-pkgs/pkg-test3.zip" "pkg-test1 pkg-test3"
                    $ "test -f test-pkgs/pkg-test3-v2.zip"
                    $ "raco pkg install test-pkgs/pkg-test3-v2.zip" =exit> 1)
   (shelly-install* "conflicts are caught in single-collection against multi-collection"
                    "test-pkgs/pkg-test1.zip test-pkgs/pkg-test3-v2.zip" "pkg-test1 pkg-test3-v2"
                    $ "test -f test-pkgs/pkg-test3.zip"
                    $ "raco pkg install test-pkgs/pkg-test3.zip" =exit> 1)

   (shelly-wind
    $ "cp -r test-pkgs/pkg-test1 test-pkgs/pkg-test1-linking"
    (shelly-install* "conflicts are caught, even with a link" 
                    "--link test-pkgs/pkg-test1-linking"
                    "pkg-test1-linking"
                    $ "test -f test-pkgs/pkg-test1-conflict.zip"
                    $ "raco pkg install test-pkgs/pkg-test1-conflict.zip" =exit> 1)
    (finally
     $ "rm -fr test-pkgs/pkg-test1-linking"))

   (shelly-install "conflicts can be forced" "test-pkgs/pkg-test1.zip"
                   $ "racket -e '(require pkg-test1/conflict)'" =exit> 42
                   $ "raco pkg install --force test-pkgs/pkg-test1-conflict.zip" =exit> 0
                   $ "racket -e '(require pkg-test1/conflict)'" =exit> 42
                   $ "raco pkg remove pkg-test1-conflict")

   (shelly-install "conflicts can be forced" "test-pkgs/pkg-test1-conflict.zip"
                   $ "racket -e '(require pkg-test1/conflict)'" =exit> 43
                   $ "raco pkg install --force test-pkgs/pkg-test1.zip" =exit> 0
                   $ "racket -e '(require pkg-test1/conflict)'" =exit> 43
                   $ "raco pkg remove pkg-test1-conflict"))))
