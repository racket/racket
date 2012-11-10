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
         planet2/util
         "shelly.rkt"
         "util.rkt")

(pkg-tests
 (shelly-begin
  (initialize-indexes)

  $ "raco pkg create test-pkgs/planet2-test1"
  $ "raco pkg create test-pkgs/planet2-test1-not-conflict"
  (shelly-install "only modules are considered for conflicts"
                  "test-pkgs/planet2-test1.plt"
                  $ "raco pkg install test-pkgs/planet2-test1-not-conflict.plt")

  (shelly-case
   "conflicts"
   (shelly-install "double install fails" "test-pkgs/planet2-test1.zip"
                   $ "raco pkg install test-pkgs/planet2-test1.zip" =exit> 1)

   (with-fake-root
    (shelly-case
     "conflicts with racket fail"
     $ "test -f test-pkgs/racket-conflict.tgz"
     $ "raco pkg install test-pkgs/racket-conflict.tgz" =exit> 1))

   (shelly-install "conflicts are caught" "test-pkgs/planet2-test1.zip"
                   $ "test -f test-pkgs/planet2-test1-conflict.zip"
                   $ "raco pkg install test-pkgs/planet2-test1-conflict.zip" =exit> 1)

   (shelly-wind
    $ "cp -r test-pkgs/planet2-test1 test-pkgs/planet2-test1-linking"
    (shelly-install* "conflicts are caught, even with a link" 
                    "--link test-pkgs/planet2-test1-linking"
                    "planet2-test1-linking"
                    $ "test -f test-pkgs/planet2-test1-conflict.zip"
                    $ "raco pkg install test-pkgs/planet2-test1-conflict.zip" =exit> 1)
    (finally
     $ "rm -fr test-pkgs/planet2-test1-linking"))

   (shelly-install "conflicts can be forced" "test-pkgs/planet2-test1.zip"
                   $ "racket -e '(require planet2-test1/conflict)'" =exit> 42
                   $ "raco pkg install --force test-pkgs/planet2-test1-conflict.zip" =exit> 0
                   $ "racket -e '(require planet2-test1/conflict)'" =exit> 42
                   $ "raco pkg remove planet2-test1-conflict")

   (shelly-install "conflicts can be forced" "test-pkgs/planet2-test1-conflict.zip"
                   $ "racket -e '(require planet2-test1/conflict)'" =exit> 43
                   $ "raco pkg install --force test-pkgs/planet2-test1.zip" =exit> 0
                   $ "racket -e '(require planet2-test1/conflict)'" =exit> 43
                   $ "raco pkg remove planet2-test1-conflict"))))
