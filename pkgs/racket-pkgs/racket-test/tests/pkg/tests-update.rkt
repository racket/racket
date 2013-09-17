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
   (shelly-case "update of bad name"
                $ "raco pkg update bad#2" =exit> 1
                =stderr> #rx"disallowed")
   (shelly-install "local packages can't be updated (file)"
                   "test-pkgs/pkg-test1.zip"
                   $ "raco pkg update pkg-test1" =exit> 1)
   (shelly-install "local packages can't be updated (directory)"
                   "test-pkgs/pkg-test1/"
                   $ "raco pkg update pkg-test1" =exit> 1)
   (shelly-wind
    $ "mkdir -p test-pkgs/update-test"
    $ "cp -f test-pkgs/pkg-test1-v2.zip test-pkgs/update-test/pkg-test1.zip"
    (shelly-install* "packages can be replaced with local packages (file)"
                     "test-pkgs/pkg-test1.zip"
                     "pkg-test1"
                     $ "racket -e '(require pkg-test1/update)'" =exit> 42
                     $ "raco pkg update test-pkgs/update-test/pkg-test1.zip"
                     $ "racket -e '(require pkg-test1/update)'" =exit> 43)
    (finally
     $ "rm -f test-pkgs/update-test/pkg-test1.zip"))
   (shelly-install "packages can be replaced with local packages (file + name)"
                   "test-pkgs/pkg-test1.zip"
                   $ "racket -e '(require pkg-test1/update)'" =exit> 42
                   $ "raco pkg update --name pkg-test1 test-pkgs/pkg-test1-v2.zip"
                   $ "racket -e '(require pkg-test1/update)'" =exit> 43)
   (shelly-install "packages can be replaced with local packages (directory)"
                   "test-pkgs/pkg-test1.zip"
                   $ "racket -e '(require pkg-test1/update)'" =exit> 42
                   $ "raco pkg update --name pkg-test1 test-pkgs/pkg-test1-v2"
                   $ "racket -e '(require pkg-test1/update)'" =exit> 43)
   (shelly-install "replacement checksum can be checked"
                   "test-pkgs/pkg-test1.zip"
                   $ "raco pkg update test-pkgs/pkg-test1.zip" =stdout> "No updates available\n")
   (shelly-install "checksum can be supplied for local directory"
                   "test-pkgs/pkg-test1.zip"
                   $ "racket -e '(require pkg-test1/update)'" =exit> 42
                   $ "raco pkg update --name pkg-test1 --checksum abcdef test-pkgs/pkg-test1-v2"
                   $ "racket -e '(require pkg-test1/update)'" =exit> 43
                   $ "raco pkg show" =stdout> #rx"abcdef"
                   $ "raco pkg update --name pkg-test1 --checksum abcdef test-pkgs/pkg-test1-v2" =stdout> "No updates available\n")

   (shelly-wind
    $ "mkdir -p test-pkgs/update-test"
    $ "cp -f test-pkgs/pkg-test1.zip test-pkgs/update-test/pkg-test1.zip"
    $ "cp -f test-pkgs/pkg-test1.zip.CHECKSUM test-pkgs/update-test/pkg-test1.zip.CHECKSUM"
    (shelly-install* "remote packages can be updated"
                     "http://localhost:9999/update-test/pkg-test1.zip"
                     "pkg-test1"
                     $ "raco pkg update pkg-test1" =exit> 0 =stdout> "Downloading checksum for pkg-test1\nNo updates available\n"
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
    (shelly-install* "remote packages can be updated, single-collection to multi-collection"
                     "test-pkgs/pkg-test1.zip http://localhost:9999/update-test/pkg-test3.zip"
                     "pkg-test1 pkg-test3"
                     $ "raco pkg update pkg-test3" =exit> 0 =stdout> "Downloading checksum for pkg-test3\nNo updates available\n"
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
                     $ "raco pkg update pkg-test3" =exit> 0 =stdout> "Downloading checksum for pkg-test3\nNo updates available\n"
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
    $ "cp -f test-pkgs/pkg-test2.zip test-pkgs/update-test/pkg-test2.zip"
    $ "cp -f test-pkgs/pkg-test2.zip.CHECKSUM test-pkgs/update-test/pkg-test2.zip.CHECKSUM"
    (shelly-install* "update deps"
                     "http://localhost:9999/update-test/pkg-test1.zip"
                     "pkg-test1"
                     $ "raco pkg install http://localhost:9999/update-test/pkg-test2.zip"
                     $ "raco pkg update --update-deps pkg-test2" =exit> 0 
                     =stdout> "Downloading checksum for pkg-test2\nDownloading checksum for pkg-test1\nNo updates available\n"
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
    $ "cp -f test-pkgs/pkg-test3.zip test-pkgs/update-test/pkg-test3.zip"
    $ "cp -f test-pkgs/pkg-test3.zip.CHECKSUM test-pkgs/update-test/pkg-test3.zip.CHECKSUM"
    (shelly-install* "update original and deps"
                     "http://localhost:9999/update-test/pkg-test1.zip"
                     "pkg-test1"
                     $ "raco pkg install http://localhost:9999/update-test/pkg-test3.zip"
                     $ "raco pkg update --update-deps pkg-test3" =exit> 0 
                     =stdout> "Downloading checksum for pkg-test3\nDownloading checksum for pkg-test1\nNo updates available\n"
                     $ "racket -e '(require pkg-test1/update)'" =exit> 42
                     $ "cp -f test-pkgs/pkg-test1-v2.zip test-pkgs/update-test/pkg-test1.zip"
                     $ "cp -f test-pkgs/pkg-test1-v2.zip.CHECKSUM test-pkgs/update-test/pkg-test1.zip.CHECKSUM"
                     $ "cp -f test-pkgs/pkg-test3-v2.zip test-pkgs/update-test/pkg-test3.zip"
                     $ "cp -f test-pkgs/pkg-test3-v2.zip.CHECKSUM test-pkgs/update-test/pkg-test3.zip.CHECKSUM"
                     $ "raco pkg update --update-deps --deps search-auto pkg-test3" =exit> 0
                     $ "racket -e '(require pkg-test1/update)'" =exit> 43
                     $ "racket -e '(require pkg-test3)'" =stdout> #rx"version 2 loaded"
                     $ "raco pkg remove pkg-test3")
    (finally
     $ "rm -f test-pkgs/update-test/pkg-test1.zip"
     $ "rm -f test-pkgs/update-test/pkg-test1.zip.CHECKSUM"))

   (shelly-wind
    $ "mkdir -p test-pkgs/update-test"
    $ "cp -f test-pkgs/pkg-test1.zip test-pkgs/update-test/pkg-test1.zip"
    $ "cp -f test-pkgs/pkg-test1.zip.CHECKSUM test-pkgs/update-test/pkg-test1.zip.CHECKSUM"
    $ "cp -f test-pkgs/pkg-test3.zip test-pkgs/update-test/pkg-test3.zip"
    $ "cp -f test-pkgs/pkg-test3.zip.CHECKSUM test-pkgs/update-test/pkg-test3.zip.CHECKSUM"
    (shelly-install* "update original, where update has no deps"
                     "http://localhost:9999/update-test/pkg-test1.zip"
                     "pkg-test1"
                     $ "raco pkg install http://localhost:9999/update-test/pkg-test3.zip"
                     $ "raco pkg update --update-deps pkg-test3" =exit> 0 
                     =stdout> "Downloading checksum for pkg-test3\nDownloading checksum for pkg-test1\nNo updates available\n"
                     $ "racket -e '(require pkg-test1/update)'" =exit> 42
                     $ "cp -f test-pkgs/pkg-test1-v2.zip test-pkgs/update-test/pkg-test1.zip"
                     $ "cp -f test-pkgs/pkg-test1-v2.zip.CHECKSUM test-pkgs/update-test/pkg-test1.zip.CHECKSUM"
                     $ "cp -f test-pkgs/pkg-test3-v3.zip test-pkgs/update-test/pkg-test3.zip"
                     $ "cp -f test-pkgs/pkg-test3-v3.zip.CHECKSUM test-pkgs/update-test/pkg-test3.zip.CHECKSUM"
                     $ "raco pkg update --update-deps pkg-test3" =exit> 0
                     $ "racket -e '(require pkg-test1/update)'" =exit> 42
                     $ "racket -e '(require pkg-test3)'" =stdout> #rx"version 3 loaded"
                     $ "raco pkg remove pkg-test3")
    (finally
     $ "rm -f test-pkgs/update-test/pkg-test1.zip"
     $ "rm -f test-pkgs/update-test/pkg-test1.zip.CHECKSUM"))

   (shelly-wind
    $ "mkdir -p test-pkgs/update-test"
    $ "cp -f test-pkgs/pkg-test1.zip test-pkgs/update-test/pkg-test1.zip"
    $ "cp -f test-pkgs/pkg-test1.zip.CHECKSUM test-pkgs/update-test/pkg-test1.zip.CHECKSUM"
    $ "cp -f test-pkgs/pkg-test3-v3.zip test-pkgs/update-test/pkg-test3.zip"
    $ "cp -f test-pkgs/pkg-test3-v3.zip.CHECKSUM test-pkgs/update-test/pkg-test3.zip.CHECKSUM"
    (shelly-install* "update and get updates for newly introduced deps"
                     "http://localhost:9999/update-test/pkg-test1.zip"
                     "pkg-test1"
                     $ "raco pkg install http://localhost:9999/update-test/pkg-test3.zip"
                     $ "racket -e '(require pkg-test3)'" =stdout> #rx"version 3 loaded"
                     $ "raco pkg update --update-deps pkg-test3" =exit> 0 
                     =stdout> "Downloading checksum for pkg-test3\nNo updates available\n"
                     $ "racket -e '(require pkg-test1/update)'" =exit> 42
                     $ "cp -f test-pkgs/pkg-test1-v2.zip test-pkgs/update-test/pkg-test1.zip"
                     $ "cp -f test-pkgs/pkg-test1-v2.zip.CHECKSUM test-pkgs/update-test/pkg-test1.zip.CHECKSUM"
                     $ "cp -f test-pkgs/pkg-test3.zip test-pkgs/update-test/pkg-test3.zip"
                     $ "cp -f test-pkgs/pkg-test3.zip.CHECKSUM test-pkgs/update-test/pkg-test3.zip.CHECKSUM"
                     $ "raco pkg update --update-deps --deps search-auto pkg-test3" =exit> 0
                     $ "racket -e '(require pkg-test1/update)'" =exit> 43
                     $ "racket -e '(require pkg-test3)'" =stdout> #rx"main loaded"
                     $ "raco pkg remove pkg-test3")
    (finally
     $ "rm -f test-pkgs/update-test/pkg-test1.zip"
     $ "rm -f test-pkgs/update-test/pkg-test1.zip.CHECKSUM"))

   (shelly-wind
    $ "mkdir -p test-pkgs/update-test"
    $ "cp -f test-pkgs/pkg-test1.zip test-pkgs/update-test/pkg-test1.zip"
    $ "cp -f test-pkgs/pkg-test1.zip.CHECKSUM test-pkgs/update-test/pkg-test1.zip.CHECKSUM"
    (shelly-install* "update all"
                     "http://localhost:9999/update-test/pkg-test1.zip"
                     "pkg-test1"
                     $ "raco pkg install test-pkgs/pkg-test2.zip"
                     $ "raco pkg update -a" =exit> 0 =stdout> "Downloading checksum for pkg-test1\nNo updates available\n"
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
     ($ "raco pkg update pkg-test1" =exit> 0 =stdout> "Resolving \"pkg-test1\" via http://localhost:9990\nNo updates available\n"
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
