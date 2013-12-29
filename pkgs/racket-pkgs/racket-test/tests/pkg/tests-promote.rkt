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
    "promote"
    $ "raco pkg config --set catalogs http://localhost:9990"
    $ "raco pkg install --deps search-auto test-pkgs/pkg-test2.zip" =exit> 0
    $ "raco pkg show -u -a" =stdout> #rx"Package\\[\\*=auto\\] +Checksum +Source\npkg-test1\\* +[a-f0-9]+ +\\(catalog pkg-test1\\)\npkg-test2 +[a-f0-9]+ +\\(file .+tests/pkg/test-pkgs/pkg-test2.zip\\)\n"
    $ "raco pkg install test-pkgs/pkg-test2.zip" =exit> 1 =stderr> #rx"already installed"
    $ "raco pkg install test-pkgs/pkg-test1.zip" =exit> 1 =stderr> #rx"already installed from a different source"
    $ "raco pkg install pkg-test1" ; promote
    $ "raco pkg show -u -a" =stdout> #rx"Package\\[\\*=auto\\] +Checksum +Source\npkg-test1 +[a-f0-9]+ +\\(catalog pkg-test1\\)\npkg-test2 +[a-f0-9]+ +\\(file .+tests/pkg/test-pkgs/pkg-test2.zip\\)\n"
    $ "raco pkg install pkg-test1" =exit> 1 =stderr> #rx"already installed" ; redundant promote fails
    $ "racket -e '(require pkg-test1)'" =exit> 0
    $ "racket -e '(require pkg-test2)'" =exit> 0
    $ "raco pkg remove --auto pkg-test1" =exit> 1 =stderr> #rx"cannot remove packages that are dependencies of other packages"
    $ "raco pkg remove --auto pkg-test2"
    $ "raco pkg show -u -a" =stdout> #rx"Package\\[\\*=auto\\] +Checksum +Source\npkg-test1 +[a-f0-9]+ +\\(catalog pkg-test1\\)"
    $ "raco pkg remove --auto pkg-test1"
    $ "raco pkg show -u -a" =stdout> " [none]\n")
   (shelly-case
    "demote"
    $ "raco pkg config --set catalogs http://localhost:9990"
    $ "raco pkg install --deps search-auto test-pkgs/pkg-test2.zip" =exit> 0
    $ "raco pkg show -u -a" =stdout> #rx"Package\\[\\*=auto\\] +Checksum +Source\npkg-test1\\* +[a-f0-9]+ +\\(catalog pkg-test1\\)\npkg-test2 +[a-f0-9]+ +\\(file .+tests/pkg/test-pkgs/pkg-test2.zip\\)\n"
    $ "raco pkg remove --demote pkg-test2"
    $ "raco pkg show -u -a" =stdout> #rx"Package\\[\\*=auto\\] +Checksum +Source\npkg-test1\\* +[a-f0-9]+ +\\(catalog pkg-test1\\)\npkg-test2\\* +[a-f0-9]+ +\\(file .+tests/pkg/test-pkgs/pkg-test2.zip\\)\n"
    $ "racket -e '(require pkg-test1)'" =exit> 0
    $ "racket -e '(require pkg-test2)'" =exit> 0
    $ "raco pkg remove --auto"
    $ "raco pkg show -u -a" =stdout> " [none]\n"))
  (with-fake-root
   (shelly-case
    "demote+auto"
    $ "raco pkg config --set catalogs http://localhost:9990"
    $ "raco pkg install --deps search-auto test-pkgs/pkg-test2.zip" =exit> 0
    $ "raco pkg show -u -a" =stdout> #rx"Package\\[\\*=auto\\] +Checksum +Source\npkg-test1\\* +[a-f0-9]+ +\\(catalog pkg-test1\\)\npkg-test2 +[a-f0-9]+ +\\(file .+tests/pkg/test-pkgs/pkg-test2.zip\\)\n"
    $ "raco pkg remove --demote --auto pkg-test1" =exit> 0 ; should have no effect
    $ "raco pkg show -u -a" =stdout> #rx"Package\\[\\*=auto\\] +Checksum +Source\npkg-test1\\* +[a-f0-9]+ +\\(catalog pkg-test1\\)\npkg-test2 +[a-f0-9]+ +\\(file .+tests/pkg/test-pkgs/pkg-test2.zip\\)\n"
    $ "raco pkg remove --demote --auto pkg-test2"
    $ "raco pkg show -u -a" =stdout> " [none]\n"))))
