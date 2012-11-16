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
   "raco pkg install tests"
   (shelly-install "local package (tgz)" "test-pkgs/planet2-test1.tgz")
   (shelly-install "local package (zip)" "test-pkgs/planet2-test1.zip")
   (shelly-install "local package (plt)" "test-pkgs/planet2-test1.plt")

   (shelly-case
    "invalid package format is an error"
    $ "raco pkg install test-pkgs/planet2-test1.zip.CHECKSUM" =exit> 1)

   (shelly-install "remote/URL/http package (file, tgz)"
                   "http://localhost:9999/planet2-test1.tgz")
   (shelly-install "remote/URL/http package (directory)"
                   "http://localhost:9999/planet2-test1/")

   (shelly-case
    "remote/URL/http directory, non-existant file"
    $ "raco pkg install http://localhost:9999/planet2-test1.rar" =exit> 1)
   (shelly-case
    "remote/URL/http directory, no manifest fail"
    $ "raco pkg install http://localhost:9999/planet2-test1/planet2-test1"
    =exit> 1
    =stderr> #rx"Invalid package format")
   (shelly-case
    "remote/URL/http directory, bad manifest"
    ;; XXX why does this error now?
    $ "raco pkg install http://localhost:9999/planet2-test1-manifest-error" =exit> 1)

   (shelly-case
    "local directory fails when not there (because interpreted as package name that isn't there)"
    $ "raco pkg install test-pkgs/planet2-test1-not-there" =exit> 1)

   (shelly-install "local package (directory)" "test-pkgs/planet2-test1")
   (shelly-install "local package (directory with slash)" "test-pkgs/planet2-test1/")

   (with-fake-root
    (shelly-case
     "linking local directory"
     (shelly-wind
      $ "cp -r test-pkgs/planet2-test1 test-pkgs/planet2-test1-linking"
      $ "racket -e '(require planet2-test1)'" =exit> 1
      $ "raco pkg install --link test-pkgs/planet2-test1-linking"
      $ "racket -e '(require planet2-test1)'"
      $ "racket -e '(require planet2-test1/a)'" =exit> 1
      $ "cp test-pkgs/planet2-test1-staging/a.rkt test-pkgs/planet2-test1-linking/planet2-test1/a.rkt"
      $ "racket -e '(require planet2-test1/a)'"
      $ "rm -f test-pkgs/planet2-test1-linking/planet2-test1/a.rkt"
      $ "racket -e '(require planet2-test1/a)'" =exit> 1
      $ "raco pkg remove planet2-test1-linking"
      $ "racket -e '(require planet2-test1)'" =exit> 1
      (finally
       $ "rm -r test-pkgs/planet2-test1-linking"))))

   (with-fake-root
    (shelly-case
     "remote/name package, doesn't work when no package there"
     $ "raco pkg config --set indexes http://localhost:9990"
     $ "raco pkg install planet2-test1-not-there" =exit> 1))

   (with-fake-root
    (shelly-case
     "remote/name package"
     $ "raco pkg config --set indexes http://localhost:9990"
     $ "racket -e '(require planet2-test1)'" =exit> 1
     $ "raco pkg install planet2-test1"
     $ "racket -e '(require planet2-test1)'"
     $ "raco pkg remove planet2-test1"
     $ "racket -e '(require planet2-test1)'" =exit> 1))

   (with-fake-root
    (shelly-case
     "remote/name package (multi)"
     $ "raco pkg config --set indexes http://localhost:9990 http://localhost:9991"
     $ "racket -e '(require planet2-test1)'" =exit> 1
     $ "raco pkg install --deps search-auto planet2-test2-snd"
     $ "racket -e '(require planet2-test1)'"
     $ "racket -e '(require planet2-test2)'"
     $ "raco pkg remove planet2-test2-snd planet2-test1"
     $ "racket -e '(require planet2-test1)'" =exit> 1)))))
