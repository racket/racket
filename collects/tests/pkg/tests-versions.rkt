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
   "create packages"
   $ "raco pkg create --format zip test-pkgs/pkg-v-one"
   $ "raco pkg create --format zip test-pkgs/pkg-v-two"
   $ "raco pkg create --format zip test-pkgs/pkg-w-one")

  (hash-set! *index-ht-1* "pkg-v"
             (hasheq 'checksum
                     (file->string "test-pkgs/pkg-v-one.zip.CHECKSUM")
                     'source
                     "http://localhost:9999/pkg-v-one.zip"))

  (hash-set! *index-ht-1* "pkg-w"
             (hasheq 'checksum
                     (file->string "test-pkgs/pkg-w-one.zip.CHECKSUM")
                     'source
                     "http://localhost:9999/pkg-w-one.zip"))
  
  $ "raco pkg config --set catalogs http://localhost:9990"

  (shelly-case
   "update"
   (shelly-begin "install pkg-v version 1.0"
                 $ "raco pkg install pkg-v")
   (shelly-begin "fail on install pkg-w, bad version for pkg-v"
                 $ "raco pkg install --deps fail pkg-w"
                 =exit> 1
                 =stderr> #rx".*version mismatch for dependency.*for package: pkg-w.*pkg-v [(]have 1[.]0, need 2[.]0[)]")
   (shelly-begin "auto-update still fails"
                 $ "raco pkg install --deps search-auto pkg-w"
                 =exit> 1
                 =stderr> #rx".*version mismatch for dependency.*for package: pkg-w.*pkg-v [(]have 1[.]0, need 2[.]0[)]"))

  (hash-set! *index-ht-1* "pkg-v"
             (hasheq 'checksum
                     (file->string "test-pkgs/pkg-v-two.zip.CHECKSUM")
                     'source
                     "http://localhost:9999/pkg-v-two.zip"))

  (shelly-case
   "update"
   (shelly-begin "auto-update now succeeds (installs and version matches)"
                 $ "raco pkg install --deps search-auto pkg-w"))

  (initialize-catalogs)))

