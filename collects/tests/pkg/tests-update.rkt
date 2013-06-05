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

  (shelly-case
   "update"
   (shelly-install "local packages can't be updated (file)"
                   "test-pkgs/pkg-test1.zip"
                   $ "raco pkg update pkg-test1" =exit> 1)
   (shelly-install "local packages can't be updated (directory)"
                   "test-pkgs/pkg-test1/"
                   $ "raco pkg update pkg-test1" =exit> 1)
   (shelly-wind
    $ "mkdir -p test-pkgs/update-test"
    $ "cp -f test-pkgs/pkg-test1.zip test-pkgs/update-test/pkg-test1.zip"
    $ "cp -f test-pkgs/pkg-test1.zip.CHECKSUM test-pkgs/update-test/pkg-test1.zip.CHECKSUM"
    (shelly-install* "remote packages can be updated"
                     "http://localhost:9999/update-test/pkg-test1.zip"
                     "pkg-test1"
                     $ "raco pkg update pkg-test1" =exit> 0 =stdout> "Downloading checksum\nNo updates available\n"
                     $ "racket -e '(require pkg-test1/update)'" =exit> 42
                     $ "cp -f test-pkgs/pkg-test1-v2.zip test-pkgs/update-test/pkg-test1.zip"
                     $ "cp -f test-pkgs/pkg-test1-v2.zip.CHECKSUM test-pkgs/update-test/pkg-test1.zip.CHECKSUM"
                     $ "raco pkg update pkg-test1" =exit> 0
                     $ "racket -e '(require pkg-test1/update)'" =exit> 43)
    (finally
     $ "rm -f test-pkgs/update-test/pkg-test1.zip"
     $ "rm -f test-pkgs/update-test/pkg-test1.zip.CHECKSUM"))

   (shelly-wind
    $ "mkdir -p test-pkgs/update-test"
    $ "cp -f test-pkgs/pkg-test3.zip test-pkgs/update-test/pkg-test3.zip"
    $ "cp -f test-pkgs/pkg-test3.zip.CHECKSUM test-pkgs/update-test/pkg-test3.zip.CHECKSUM"
    (shelly-install* "remote packages can be updated, single-colelction to multi-collection"
                     "test-pkgs/pkg-test1.zip http://localhost:9999/update-test/pkg-test3.zip"
                     "pkg-test1 pkg-test3"
                     $ "raco pkg update pkg-test3" =exit> 0 =stdout> "Downloading checksum\nNo updates available\n"
                     $ "cp -f test-pkgs/pkg-test3-v2.zip test-pkgs/update-test/pkg-test3.zip"
                     $ "cp -f test-pkgs/pkg-test3-v2.zip.CHECKSUM test-pkgs/update-test/pkg-test3.zip.CHECKSUM"
                     $ "raco pkg update pkg-test3" =exit> 0
                     $ "racket -e '(require pkg-test3)'" =stdout> "pkg-test3/main version 2 loaded\n")
    (finally
     $ "rm -f test-pkgs/update-test/pkg-test3.zip"
     $ "rm -f test-pkgs/update-test/pkg-test3.zip.CHECKSUM"))

   (shelly-wind
    $ "mkdir -p test-pkgs/update-test"
    $ "cp -f test-pkgs/pkg-test3-v2.zip test-pkgs/update-test/pkg-test3.zip"
    $ "cp -f test-pkgs/pkg-test3-v2.zip.CHECKSUM test-pkgs/update-test/pkg-test3.zip.CHECKSUM"
    (shelly-install* "remote packages can be updated, multi-colelction to single-collection"
                     "test-pkgs/pkg-test1.zip http://localhost:9999/update-test/pkg-test3.zip"
                     "pkg-test1 pkg-test3"
                     $ "raco pkg update pkg-test3" =exit> 0 =stdout> "Downloading checksum\nNo updates available\n"
                     $ "cp -f test-pkgs/pkg-test3.zip test-pkgs/update-test/pkg-test3.zip"
                     $ "cp -f test-pkgs/pkg-test3.zip.CHECKSUM test-pkgs/update-test/pkg-test3.zip.CHECKSUM"
                     $ "raco pkg update pkg-test3" =exit> 0
                     $ "racket -e '(require pkg-test3)'" =stdout> "pkg-test3/main loaded\n")
    (finally
     $ "rm -f test-pkgs/update-test/pkg-test3.zip"
     $ "rm -f test-pkgs/update-test/pkg-test3.zip.CHECKSUM"))

   (shelly-wind
    $ "mkdir -p test-pkgs/update-test"
    $ "cp -f test-pkgs/pkg-test1.zip test-pkgs/update-test/pkg-test1.zip"
    $ "cp -f test-pkgs/pkg-test1.zip.CHECKSUM test-pkgs/update-test/pkg-test1.zip.CHECKSUM"
    (shelly-install* "update deps"
                     "http://localhost:9999/update-test/pkg-test1.zip"
                     "pkg-test1"
                     $ "raco pkg install test-pkgs/pkg-test2.zip"
                     $ "raco pkg update --update-deps pkg-test2" =exit> 0 =stdout> "Downloading checksum\nNo updates available\n"
                     $ "racket -e '(require pkg-test1/update)'" =exit> 42
                     $ "cp -f test-pkgs/pkg-test1-v2.zip test-pkgs/update-test/pkg-test1.zip"
                     $ "cp -f test-pkgs/pkg-test1-v2.zip.CHECKSUM test-pkgs/update-test/pkg-test1.zip.CHECKSUM"
                     $ "raco pkg update --update-deps pkg-test2" =exit> 0
                     $ "racket -e '(require pkg-test1/update)'" =exit> 43
                     $ "raco pkg remove pkg-test2")
    (finally
     $ "rm -f test-pkgs/update-test/pkg-test1.zip"
     $ "rm -f test-pkgs/update-test/pkg-test1.zip.CHECKSUM"))

   (shelly-wind
    $ "mkdir -p test-pkgs/update-test"
    $ "cp -f test-pkgs/pkg-test1.zip test-pkgs/update-test/pkg-test1.zip"
    $ "cp -f test-pkgs/pkg-test1.zip.CHECKSUM test-pkgs/update-test/pkg-test1.zip.CHECKSUM"
    (shelly-install* "update all is default"
                     "http://localhost:9999/update-test/pkg-test1.zip"
                     "pkg-test1"
                     $ "raco pkg install test-pkgs/pkg-test2.zip"
                     $ "raco pkg update -a" =exit> 0 =stdout> "Downloading checksum\nNo updates available\n"
                     $ "racket -e '(require pkg-test1/update)'" =exit> 42
                     $ "cp -f test-pkgs/pkg-test1-v2.zip test-pkgs/update-test/pkg-test1.zip"
                     $ "cp -f test-pkgs/pkg-test1-v2.zip.CHECKSUM test-pkgs/update-test/pkg-test1.zip.CHECKSUM"
                     $ "raco pkg update -a" =exit> 0
                     $ "racket -e '(require pkg-test1/update)'" =exit> 43
                     $ "raco pkg remove pkg-test2")
    (finally
     $ "rm -f test-pkgs/update-test/pkg-test1.zip"
     $ "rm -f test-pkgs/update-test/pkg-test1.zip.CHECKSUM"))

   (shelly-wind
    $ "cp -f test-pkgs/pkg-test1.zip test-pkgs/pkg-test1.zip.bak"
    $ "cp -f test-pkgs/pkg-test1.zip.CHECKSUM test-pkgs/pkg-test1.zip.CHECKSUM.bak"
    (shelly-install**
     "named remote packages can be update"
     "pkg-test1" "pkg-test1"
     ($ "raco pkg config --set catalogs http://localhost:9990")
     ($ "raco pkg update pkg-test1" =exit> 0 =stdout> "No updates available\n"
        $ "racket -e '(require pkg-test1/update)'" =exit> 42
        $ "cp test-pkgs/pkg-test1-v2.zip test-pkgs/pkg-test1.zip"
        $ "cp test-pkgs/pkg-test1-v2.zip.CHECKSUM test-pkgs/pkg-test1.zip.CHECKSUM"
        (initialize-catalogs)
        $ "raco pkg update pkg-test1" =exit> 0
        $ "racket -e '(require pkg-test1/update)'" =exit> 43))
    (finally
     $ "cp -f test-pkgs/pkg-test1.zip.bak test-pkgs/pkg-test1.zip"
     $ "cp -f test-pkgs/pkg-test1.zip.CHECKSUM.bak test-pkgs/pkg-test1.zip.CHECKSUM"
     (initialize-catalogs))))))
