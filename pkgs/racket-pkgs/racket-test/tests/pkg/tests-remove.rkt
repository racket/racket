#lang racket/base
(require rackunit
         racket/system
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

(this-test-is-run-by-the-main-test)

(pkg-tests
 (shelly-begin
  (initialize-catalogs)
  
  (shelly-case
   "remove and show"
   (shelly-case "remove of not installed package fails"
                $ "raco pkg show -u -a" =stdout> " [none]\n"
                $ "raco pkg remove not-there" =exit> 1)
   (shelly-case "remove of bad name"
                $ "raco pkg remove bad/" =exit> 1
                =stderr> #rx"disallowed")
   (shelly-case "remove of bad name"
                $ "raco pkg remove bad#2" =exit> 1
                =stderr> #rx"disallowed")
   (shelly-install "remove test"
                   "test-pkgs/pkg-test1.zip")
   (shelly-install "remove test with immediately redundant package name"
                   "test-pkgs/pkg-test1.zip"
                   "pkg-test1 pkg-test1")
   (shelly-install "remove of dep fails"
                   "test-pkgs/pkg-test1.zip"
                   $ "raco pkg show -u -a" =stdout> #rx"Package\\[\\*=auto\\] +Checksum +Source\npkg-test1 +[a-f0-9]+ +\\(file .+tests/pkg/test-pkgs/pkg-test1.zip\\)\n"
                   $ "raco pkg install test-pkgs/pkg-test2.zip"
                   $ "raco pkg show -u -a" =stdout> #rx"Package\\[\\*=auto\\] +Checksum +Source\npkg-test1 +[a-f0-9]+ +\\(file .+tests/pkg/test-pkgs/pkg-test1.zip\\)\npkg-test2 +[a-f0-9]+ +\\(file .+tests/pkg/test-pkgs/pkg-test2.zip\\)\n"
                   $ "raco pkg remove pkg-test1" =exit> 1 =stderr> #rx"pkg-test1 \\(required by: \\(pkg-test2\\)\\)"
                   $ "raco pkg remove pkg-test2"
                   $ "raco pkg show -u -a" =stdout>  #rx"Package\\[\\*=auto\\] +Checksum +Source\npkg-test1 +[a-f0-9]+ +\\(file .+tests/pkg/test-pkgs/pkg-test1.zip\\)\n")
   (shelly-install "remove of dep can be forced"
                   "test-pkgs/pkg-test1.zip"
                   $ "raco pkg install test-pkgs/pkg-test2.zip"
                   $ "racket -e '(require pkg-test2/contains-dep)'" =exit> 0
                   $ "raco pkg remove --force pkg-test1"
                   $ "racket -e '(require pkg-test2/contains-dep)'" =exit> 1
                   $ "raco pkg install test-pkgs/pkg-test1.zip"
                   $ "raco pkg remove pkg-test2")
   (with-fake-root
    (shelly-case
     "remove two"
     $ "racket -e '(require pkg-test1)'" =exit> 1
     $ "racket -e '(require pkg-test2)'" =exit> 1
     $ "raco pkg install test-pkgs/pkg-test2.zip test-pkgs/pkg-test1.zip" =exit> 0
     $ "racket -e '(require pkg-test1)'" =exit> 0
     $ "racket -e '(require pkg-test2)'" =exit> 0
     $ "racket -e '(require pkg-test2/contains-dep)'" =exit> 0
     $ "raco pkg remove pkg-test1 pkg-test2"
     $ "racket -e '(require pkg-test1)'" =exit> 1
     $ "racket -e '(require pkg-test2)'" =exit> 1))
   (with-fake-root 
    (shelly-case
     "autoremove"
     $ "raco pkg config --set catalogs http://localhost:9990"
     $ "racket -e '(require pkg-test1)'" =exit> 1
     $ "racket -e '(require pkg-test2)'" =exit> 1
     $ "raco pkg install --deps search-auto test-pkgs/pkg-test2.zip" =exit> 0
     $ "raco pkg show -u -a" =stdout> #rx"Package\\[\\*=auto\\] +Checksum +Source\npkg-test1\\* +[a-f0-9]+ +\\(catalog pkg-test1\\)\npkg-test2 +[a-f0-9]+ +\\(file .+tests/pkg/test-pkgs/pkg-test2.zip\\)\n"
     $ "racket -e '(require pkg-test1)'" =exit> 0
     $ "racket -e '(require pkg-test2)'" =exit> 0
     $ "racket -e '(require pkg-test2/contains-dep)'" =exit> 0
     $ "raco pkg remove pkg-test2"
     $ "raco pkg show -u -a" =stdout> #rx"Package\\[\\*=auto\\] +Checksum +Source\npkg-test1\\* +[a-f0-9]+ +\\(catalog pkg-test1\\)\n"
     $ "racket -e '(require pkg-test1)'" =exit> 0
     $ "raco pkg remove --auto"
     $ "raco pkg show -u -a" =stdout> " [none]\n"
     $ "racket -e '(require pkg-test1)'" =exit> 1
     $ "racket -e '(require pkg-test2)'" =exit> 1)
    (shelly-case
     "single-step autoremove"
     $ "raco pkg install --deps search-auto test-pkgs/pkg-test2.zip" =exit> 0
     $ "raco pkg remove --auto pkg-test2"
     $ "raco pkg show -u -a" =stdout> " [none]\n"
     $ "racket -e '(require pkg-test1)'" =exit> 1
     $ "racket -e '(require pkg-test2)'" =exit> 1)
    (shelly-case
     "single-step autoremove with cycles"
     $ "raco pkg install --deps search-auto --copy test-pkgs/pkg-cycle1" =exit> 0
     $ "racket -e '(require pkg-cycle1)'" =exit> 0
     $ "racket -e '(require pkg-cycle2)'" =exit> 0
     $ "raco pkg remove --auto pkg-cycle1"
     $ "raco pkg show -u -a" =stdout> " [none]\n"
     $ "racket -e '(require pkg-cycle1)'" =exit> 1
     $ "racket -e '(require pkg-cycle2)'" =exit> 1))
   (with-fake-root
    (shelly-case
     "different scope error"
     $ "raco pkg install test-pkgs/pkg-test1.zip" =exit> 0
     $ "raco pkg remove --installation pkg-test1" =exit> 1
     =stderr> #rx"package installed in a different scope"
     $ "raco pkg remove pkg-test1")))))
