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
  (shelly-install "remote/github"
                  "github://github.com/jeapostrophe/galaxy/master/tests/planet2/test-pkgs/planet2-test1")
  (shelly-install "remote/github with slash"
                  "github://github.com/jeapostrophe/galaxy/master/tests/planet2/test-pkgs/planet2-test1/")
  (shelly-install "remote/github with auto prefix"
                  "--type github jeapostrophe/galaxy/master/tests/planet2/test-pkgs/planet2-test1/")

  (hash-set! *index-ht-1* "planet2-test1-github-different-checksum"
             (hasheq 'checksum
                     "23eeaee731e72a39bddbacdf1ed6cce3bcf423a5"
                     'source
                     "github://github.com/jeapostrophe/galaxy/master/tests/planet2/test-pkgs/planet2-test1/"))

  (with-fake-root
    (shelly-case
     "remote/name package"
     $ "raco pkg config --set catalogs http://localhost:9990"
     $ "racket -e '(require planet2-test1)'" =exit> 1
     $ "raco pkg install planet2-test1-github-different-checksum"
     $ "racket -e '(require planet2-test1)'"
     $ "raco pkg remove planet2-test1-github-different-checksum"
     $ "racket -e '(require planet2-test1)'" =exit> 1))))
