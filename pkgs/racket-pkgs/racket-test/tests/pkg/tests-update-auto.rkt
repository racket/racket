#lang racket/base
(require racket/file
         "shelly.rkt"
         "util.rkt")

(this-test-is-run-by-the-main-test)

(define (init-update-deps-test)
  (shelly-begin
   (hash-set! *index-ht-1* "pkg-b"
              (hasheq 'checksum
                      (file->string "test-pkgs/pkg-b-first.plt.CHECKSUM")
                      'source
                      "http://localhost:9999/pkg-b-first.plt"))
   $ "raco pkg config --set catalogs http://localhost:9990"
   $ "raco pkg install pkg-b"
   $ "racket -e '(require pkg-b)'" =exit> 42
   (hash-set! *index-ht-1* "pkg-b"
              (hasheq 'checksum
                      (file->string "test-pkgs/pkg-b-second.plt.CHECKSUM")
                      'source
                      "http://localhost:9999/pkg-b-second.plt"))
   (hash-set! *index-ht-1* "pkg-a"
              (hasheq 'checksum
                      (file->string "test-pkgs/pkg-a-first.plt.CHECKSUM")
                      'source
                      "http://localhost:9999/pkg-a-first.plt"))))

(pkg-tests
 (shelly-case
  "create packages"
  $ "raco pkg create --format plt test-pkgs/pkg-b-second"
  $ "raco pkg create --format plt test-pkgs/pkg-a-first"
  $ "raco pkg create --format plt test-pkgs/pkg-a-second")

 (with-fake-root
  (shelly-case
   "update and then remove an auto"
   $ "raco pkg config --set catalogs http://localhost:9990"
   (hash-set! *index-ht-1* "pkg-b"
              (hasheq 'checksum
                      (file->string "test-pkgs/pkg-b-second.plt.CHECKSUM")
                      'source
                      "http://localhost:9999/pkg-b-second.plt"))
   (hash-set! *index-ht-1* "pkg-a"
              (hasheq 'checksum
                      (file->string "test-pkgs/pkg-a-first.plt.CHECKSUM")
                      'source
                      "http://localhost:9999/pkg-a-first.plt"))
   $ "raco pkg install --deps search-auto pkg-b" =exit> 0 <input= "y\n"
   $ "raco pkg show -u -a" =stdout> #rx"Package\\[\\*=auto\\] +Checksum +Source\npkg-a\\* +[a-f0-9]+    \\(catalog pkg-a\\)\npkg-b +[a-f0-9]+ +\\(catalog pkg-b\\)\n"
   $ "racket -e '(require pkg-b)'" =exit> 43
   $ "racket -e '(require pkg-a)'" =exit> 0
   ;; remove auto doesn't do anything because everything is needed
   $ "raco pkg remove -u --auto"
   $ "raco pkg show -u -a" =stdout> #rx"Package\\[\\*=auto\\] +Checksum +Source\npkg-a\\* +[a-f0-9]+    \\(catalog pkg-a\\)\npkg-b +[a-f0-9]+ +\\(catalog pkg-b\\)\n"
   $ "racket -e '(require pkg-b)'" =exit> 43
   $ "racket -e '(require pkg-a)'" =exit> 0
   ;; pkg-a is now an auto
   (hash-set! *index-ht-1* "pkg-a"
              (hasheq 'checksum
                      (file->string "test-pkgs/pkg-a-second.plt.CHECKSUM")
                      'source
                      "http://localhost:9999/pkg-a-second.plt"))
   $ "raco pkg update -a" =exit> 0
   $ "racket -e '(require pkg-a)'" =exit> 43
   $ "raco pkg remove pkg-b"
   $ "raco pkg show -u -a" =stdout> #rx"Package\\[\\*=auto\\] +Checksum +Source\npkg-a\\* +[a-f0-9]+ +\\(catalog pkg-a\\)\n"
   $ "racket -e '(require pkg-b)'" =exit> 1
   ;; pkg-a is now not needed
   $ "raco pkg remove --auto"
   $ "raco pkg show -u -a" =stdout> " [none]\n"
   $ "racket -e '(require pkg-a)'" =exit> 1)))
