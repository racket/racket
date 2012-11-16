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
   "remove and show"
   (shelly-case "remove of not installed package fails"
                $ "raco pkg show" =stdout> "Package(auto?)    Checksum    Source\n"
                $ "raco pkg remove not-there" =exit> 1)
   (shelly-install "remove test"
                   "test-pkgs/planet2-test1.zip")
   (shelly-install "remove of dep fails"
                   "test-pkgs/planet2-test1.zip"
                   $ "raco pkg show" =stdout> #rx"Package\\(auto\\?\\) +Checksum +Source\nplanet2-test1 +[a-f0-9]+ +\\(file .+tests/planet2/test-pkgs/planet2-test1.zip\\)\n"
                   $ "raco pkg install test-pkgs/planet2-test2.zip"
                   $ "raco pkg show" =stdout> #rx"Package\\(auto\\?\\) +Checksum +Source\nplanet2-test1 +[a-f0-9]+ +\\(file .+tests/planet2/test-pkgs/planet2-test1.zip\\)\nplanet2-test2 +[a-f0-9]+ +\\(file .+tests/planet2/test-pkgs/planet2-test2.zip\\)\n"
                   $ "raco pkg remove planet2-test1" =exit> 1
                   $ "raco pkg remove planet2-test2"
                   $ "raco pkg show" =stdout>  #rx"Package\\(auto\\?\\) +Checksum +Source\nplanet2-test1 +[a-f0-9]+ +\\(file .+tests/planet2/test-pkgs/planet2-test1.zip\\)\n")
   (shelly-install "remove of dep can be forced"
                   "test-pkgs/planet2-test1.zip"
                   $ "raco pkg install test-pkgs/planet2-test2.zip"
                   $ "racket -e '(require planet2-test2/contains-dep)'" =exit> 0
                   $ "raco pkg remove --force planet2-test1"
                   $ "racket -e '(require planet2-test2/contains-dep)'" =exit> 1
                   $ "raco pkg install test-pkgs/planet2-test1.zip"
                   $ "raco pkg remove planet2-test2")
   (with-fake-root
    (shelly-case
     "remove two"
     $ "racket -e '(require planet2-test1)'" =exit> 1
     $ "racket -e '(require planet2-test2)'" =exit> 1
     $ "raco pkg install test-pkgs/planet2-test2.zip test-pkgs/planet2-test1.zip" =exit> 0
     $ "racket -e '(require planet2-test1)'" =exit> 0
     $ "racket -e '(require planet2-test2)'" =exit> 0
     $ "racket -e '(require planet2-test2/contains-dep)'" =exit> 0
     $ "raco pkg remove planet2-test1 planet2-test2"
     $ "racket -e '(require planet2-test1)'" =exit> 1
     $ "racket -e '(require planet2-test2)'" =exit> 1))
   (with-fake-root
    (shelly-case
     "autoremove"
     $ "raco pkg config --set indexes http://localhost:9990"
     $ "racket -e '(require planet2-test1)'" =exit> 1
     $ "racket -e '(require planet2-test2)'" =exit> 1
     $ "raco pkg install --deps search-auto test-pkgs/planet2-test2.zip" =exit> 0
     $ "raco pkg show" =stdout> #rx"Package\\(auto\\?\\) +Checksum +Source\nplanet2-test1\\* +[a-f0-9]+ +\\(pns planet2-test1\\)\nplanet2-test2 +[a-f0-9]+ +\\(file .+tests/planet2/test-pkgs/planet2-test2.zip\\)\n"
     $ "racket -e '(require planet2-test1)'" =exit> 0
     $ "racket -e '(require planet2-test2)'" =exit> 0
     $ "racket -e '(require planet2-test2/contains-dep)'" =exit> 0
     $ "raco pkg remove planet2-test2"
     $ "raco pkg show" =stdout> #rx"Package\\(auto\\?\\) +Checksum +Source\nplanet2-test1\\* +[a-f0-9]+ +\\(pns planet2-test1\\)\n"
     $ "racket -e '(require planet2-test1)'" =exit> 0
     $ "raco pkg remove --auto"
     $ "raco pkg show" =stdout> "Package(auto?)    Checksum    Source\n"
     $ "racket -e '(require planet2-test1)'" =exit> 1
     $ "racket -e '(require planet2-test2)'" =exit> 1)))))
