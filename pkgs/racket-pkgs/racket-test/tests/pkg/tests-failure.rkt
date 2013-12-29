#lang racket/base
(require rackunit
         racket/system
         pkg/util
         "shelly.rkt"
         "util.rkt")

(this-test-is-run-by-the-main-test)

(pkg-tests
 (shelly-begin
  (initialize-catalogs)
  
  (with-fake-root
   (shelly-case
    "failure on remove"
    $ "raco pkg install test-pkgs/pkg-test1.zip" =exit> 0
    $ "raco pkg show -u -a -d" =stdout> #rx"Package\\[\\*=auto\\] +Checksum +Source +Directory\npkg-test1 +[a-f0-9]+ .*pkg-test1\n"
    $ "racket -e '(require pkg-test1)'" =exit> 0
    $ "racket -e '(file-or-directory-permissions (collection-path \"pkg-test1\") #o500)'"
    $ "raco pkg remove pkg-test1" =exit> 1
    $ "racket -e '(require pkg-test1)'" =exit> 1)

   (shelly-case
    "re-install must go to \"+1\""
    $ "raco pkg install test-pkgs/pkg-test1.zip" =exit> 0
    $ "raco pkg show -u -a -d" =stdout> #rx"Package\\[\\*=auto\\] +Checksum +Source +Directory\npkg-test1 +[a-f0-9]+ .*pkg-test1[+]1\n"
    $ "racket -e '(require pkg-test1)'" =exit> 0
    $ "raco pkg remove pkg-test1" =exit> 0
    $ "racket -e '(require pkg-test1)'" =exit> 1)

   (shelly-case
    "re-install can go back to original place"
    $ "racket -l racket/base -l setup/dirs -e '(file-or-directory-permissions (build-path (find-user-pkgs-dir) \"pkg-test1/pkg-test1\") #o700)'"
    $ "raco pkg install test-pkgs/pkg-test1.zip" =exit> 0
    $ "raco pkg show -u -a -d" =stdout> #rx"Package\\[\\*=auto\\] +Checksum +Source +Directory\npkg-test1 +[a-f0-9]+ .*pkg-test1\n"
    $ "racket -e '(require pkg-test1)'" =exit> 0
    $ "raco pkg remove pkg-test1" =exit> 0))))
