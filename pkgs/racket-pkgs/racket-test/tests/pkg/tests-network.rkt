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

;; todo: to move the test packages to the "plt" account on GitHub

(pkg-tests
 (shelly-begin
  (shelly-install
   "remote/github" "git://github.com/mflatt/pkg-test?path=pkg-test1"
   $ "racket -l racket/base -l pkg-test1/number -e '(number)'" =stdout> "1\n")
  (shelly-install "remote/github with slash"
                  "git://github.com/mflatt/pkg-test?path=pkg-test1/")
  (shelly-install 
   "remote/github with auto prefix and with branch"
   "--type github mflatt/pkg-test?path=pkg-test1/#alt"
   $ "racket -l racket/base -l pkg-test1/number -e '(number)'" =stdout> "10\n")
  (shelly-install 
   "remote/github with tag"
   "git://github.com/mflatt/pkg-test?path=pkg-test1/#hundred"
   $ "racket -l racket/base -l pkg-test1/number -e '(number)'" =stdout> "100\n")
  (shelly-install 
   "remote/github with commit"
   "git://github.com/mflatt/pkg-test?path=pkg-test1/#f9b4eef22"
   $ "racket -l racket/base -l pkg-test1/number -e '(number)'" =stdout> "100\n")
  (shelly-install 
   "remote/github with checksum"
   "--checksum f9b4eef22cdd9ab88b254cb027fc1ebe7fb596fd git://github.com/mflatt/pkg-test?path=pkg-test1"
   $ "racket -l racket/base -l pkg-test1/number -e '(number)'" =stdout> "100\n"
   $ "raco pkg update pkg-test1"
   $ "racket -l racket/base -l pkg-test1/number -e '(number)'" =stdout> "1\n")

  (hash-set! *index-ht-1* "pkg-test1-github-different-checksum"
             (hasheq 'checksum
                     "f9b4eef22cdd9ab88b254cb027fc1ebe7fb596fd"
                     'source
                     "git://github.com/mflatt/pkg-test?path=pkg-test1"))

  (with-fake-root
    (shelly-case
     "remote/name package"
     $ "raco pkg config --set catalogs http://localhost:9990"
     $ "racket -l pkg-test1/number" =exit> 1
     $ "raco pkg install pkg-test1-github-different-checksum"
     $ "racket -l racket/base -l pkg-test1/number -e '(number)'" =stdout> "100\n"
     $ "raco pkg remove pkg-test1-github-different-checksum"
     $ "racket -l pkg-test1/number" =exit> 1))))
