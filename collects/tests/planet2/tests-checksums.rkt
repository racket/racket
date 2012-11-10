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
   "checksums"
   $ "test -f test-pkgs/planet2-test1.zip"
   $ "cp -f test-pkgs/planet2-test1.zip test-pkgs/planet2-test1-bad-checksum.zip"
   $ "test -f test-pkgs/planet2-test1-conflict.zip.CHECKSUM"
   $ "cp -f test-pkgs/planet2-test1-conflict.zip.CHECKSUM test-pkgs/planet2-test1-bad-checksum.zip.CHECKSUM"
   (with-fake-root
    (shelly-case
     "checksums are checked if present (local)"
     $ "racket -e '(require planet2-test1)'" =exit> 1
     $ "raco pkg install test-pkgs/planet2-test1-bad-checksum.zip" =exit> 1
     $ "racket -e '(require planet2-test1)'" =exit> 1))
   $ "cp -f test-pkgs/planet2-test1.zip test-pkgs/planet2-test1-no-checksum.zip"

   (shelly-install* "checksums are ignored if missing by default (local)"
                    "test-pkgs/planet2-test1-no-checksum.zip"
                    "planet2-test1-no-checksum")

   (with-fake-root
    (shelly-case
     "checksums are checked (remote, indexed)"
     (hash-set!
      *index-ht-1* "planet2-test1"
      (hasheq 'checksum
              (file->string "test-pkgs/planet2-test1-bad-checksum.zip.CHECKSUM")
              'source
              "http://localhost:9999/planet2-test1-bad-checksum.zip"))
     $ "raco pkg config --set indexes http://localhost:9990 http://localhost:9991"
     $ "racket -e '(require planet2-test1)'" =exit> 1
     $ "raco pkg install planet2-test1" =exit> 1
     $ "racket -e '(require planet2-test1)'" =exit> 1))

   (with-fake-root
    (shelly-case
     "checksums are checked (remote)"
     $ "racket -e '(require planet2-test1)'" =exit> 1
     $ "raco pkg install http://localhost:9999/planet2-test1-bad-checksum.zip" =exit> 1
     $ "racket -e '(require planet2-test1)'" =exit> 1))
   (with-fake-root
    (shelly-case
     "checksums are required by default remotely (remote)"
     $ "racket -e '(require planet2-test1)'" =exit> 1
     $ "raco pkg install http://localhost:9999/planet2-test1-no-checksum.zip" =exit> 1
     $ "racket -e '(require planet2-test1)'" =exit> 1))
   (shelly-install* "but, bad checksums can be ignored (local)"
                    "--ignore-checksums test-pkgs/planet2-test1-bad-checksum.zip"
                    "planet2-test1-bad-checksum")
   (shelly-install* "but, bad checksums can be ignored (remote)"
                    "--ignore-checksums http://localhost:9999/planet2-test1-bad-checksum.zip"
                    "planet2-test1-bad-checksum")
   (shelly-install* "but, checksums can be missing if ignored (remote)"
                    "--ignore-checksums http://localhost:9999/planet2-test1-no-checksum.zip"
                    "planet2-test1-no-checksum"))))
