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

  (shelly-case
   "update"
   (shelly-install "local packages can't be updated (file)"
                   "test-pkgs/planet2-test1.zip"
                   $ "raco pkg update planet2-test1" =exit> 1)
   (shelly-install "local packages can't be updated (directory)"
                   "test-pkgs/planet2-test1"
                   $ "raco pkg update planet2-test1" =exit> 1)
   (shelly-wind
    $ "mkdir -p test-pkgs/update-test"
    $ "cp -f test-pkgs/planet2-test1.zip test-pkgs/update-test/planet2-test1.zip"
    $ "cp -f test-pkgs/planet2-test1.zip.CHECKSUM test-pkgs/update-test/planet2-test1.zip.CHECKSUM"
    (shelly-install* "remote packages can be updated"
                     "http://localhost:9999/update-test/planet2-test1.zip"
                     "planet2-test1"
                     $ "raco pkg update planet2-test1" =exit> 0 =stdout> "No updates available\n"
                     $ "racket -e '(require planet2-test1/update)'" =exit> 42
                     $ "cp -f test-pkgs/planet2-test1-v2.zip test-pkgs/update-test/planet2-test1.zip"
                     $ "cp -f test-pkgs/planet2-test1-v2.zip.CHECKSUM test-pkgs/update-test/planet2-test1.zip.CHECKSUM"
                     $ "raco pkg update planet2-test1" =exit> 0
                     $ "racket -e '(require planet2-test1/update)'" =exit> 43)
    (finally
     $ "rm -f test-pkgs/update-test/planet2-test1.zip"
     $ "rm -f test-pkgs/update-test/planet2-test1.zip.CHECKSUM"))

   (shelly-wind
    $ "mkdir -p test-pkgs/update-test"
    $ "cp -f test-pkgs/planet2-test1.zip test-pkgs/update-test/planet2-test1.zip"
    $ "cp -f test-pkgs/planet2-test1.zip.CHECKSUM test-pkgs/update-test/planet2-test1.zip.CHECKSUM"
    (shelly-install* "update deps"
                     "http://localhost:9999/update-test/planet2-test1.zip"
                     "planet2-test1"
                     $ "raco pkg install test-pkgs/planet2-test2.zip"
                     $ "raco pkg update --update-deps planet2-test2" =exit> 0 =stdout> "No updates available\n"
                     $ "racket -e '(require planet2-test1/update)'" =exit> 42
                     $ "cp -f test-pkgs/planet2-test1-v2.zip test-pkgs/update-test/planet2-test1.zip"
                     $ "cp -f test-pkgs/planet2-test1-v2.zip.CHECKSUM test-pkgs/update-test/planet2-test1.zip.CHECKSUM"
                     $ "raco pkg update --update-deps planet2-test2" =exit> 0
                     $ "racket -e '(require planet2-test1/update)'" =exit> 43
                     $ "raco pkg remove planet2-test2")
    (finally
     $ "rm -f test-pkgs/update-test/planet2-test1.zip"
     $ "rm -f test-pkgs/update-test/planet2-test1.zip.CHECKSUM"))

   (shelly-wind
    $ "mkdir -p test-pkgs/update-test"
    $ "cp -f test-pkgs/planet2-test1.zip test-pkgs/update-test/planet2-test1.zip"
    $ "cp -f test-pkgs/planet2-test1.zip.CHECKSUM test-pkgs/update-test/planet2-test1.zip.CHECKSUM"
    (shelly-install* "update all is default"
                     "http://localhost:9999/update-test/planet2-test1.zip"
                     "planet2-test1"
                     $ "raco pkg install test-pkgs/planet2-test2.zip"
                     $ "raco pkg update -a" =exit> 0 =stdout> "No updates available\n"
                     $ "racket -e '(require planet2-test1/update)'" =exit> 42
                     $ "cp -f test-pkgs/planet2-test1-v2.zip test-pkgs/update-test/planet2-test1.zip"
                     $ "cp -f test-pkgs/planet2-test1-v2.zip.CHECKSUM test-pkgs/update-test/planet2-test1.zip.CHECKSUM"
                     $ "raco pkg update -a" =exit> 0
                     $ "racket -e '(require planet2-test1/update)'" =exit> 43
                     $ "raco pkg remove planet2-test2")
    (finally
     $ "rm -f test-pkgs/update-test/planet2-test1.zip"
     $ "rm -f test-pkgs/update-test/planet2-test1.zip.CHECKSUM"))

   (shelly-wind
    $ "cp -f test-pkgs/planet2-test1.zip test-pkgs/planet2-test1.zip.bak"
    $ "cp -f test-pkgs/planet2-test1.zip.CHECKSUM test-pkgs/planet2-test1.zip.CHECKSUM.bak"
    (shelly-install**
     "named remote packages can be update"
     "planet2-test1" "planet2-test1"
     ($ "raco pkg config --set indexes http://localhost:9990")
     ($ "raco pkg update planet2-test1" =exit> 0 =stdout> "No updates available\n"
        $ "racket -e '(require planet2-test1/update)'" =exit> 42
        $ "cp test-pkgs/planet2-test1-v2.zip test-pkgs/planet2-test1.zip"
        $ "cp test-pkgs/planet2-test1-v2.zip.CHECKSUM test-pkgs/planet2-test1.zip.CHECKSUM"
        (initialize-indexes)
        $ "raco pkg update planet2-test1" =exit> 0
        $ "racket -e '(require planet2-test1/update)'" =exit> 43))
    (finally
     $ "cp -f test-pkgs/planet2-test1.zip.bak test-pkgs/planet2-test1.zip"
     $ "cp -f test-pkgs/planet2-test1.zip.CHECKSUM.bak test-pkgs/planet2-test1.zip.CHECKSUM"
     (initialize-indexes))))))
