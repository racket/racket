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
         file/zip
         file/unzip
         "shelly.rkt"
         "util.rkt")

(this-test-is-run-by-the-main-test)

(pkg-tests
 (shelly-begin
  (initialize-catalogs)

  (shelly-case
   "checksums"
   $ "test -f test-pkgs/pkg-test1.zip"
   $ "cp -f test-pkgs/pkg-test1.zip test-pkgs/pkg-test1-bad-checksum.zip"
   $ "test -f test-pkgs/pkg-test1-conflict.zip.CHECKSUM"
   $ "cp -f test-pkgs/pkg-test1-conflict.zip.CHECKSUM test-pkgs/pkg-test1-bad-checksum.zip.CHECKSUM"
   (with-fake-root
    (shelly-case
     "checksums are checked if present (local)"
     $ "racket -e '(require pkg-test1)'" =exit> 1
     $ "raco pkg install test-pkgs/pkg-test1-bad-checksum.zip" =exit> 1
     $ "racket -e '(require pkg-test1)'" =exit> 1))
   $ "cp -f test-pkgs/pkg-test1.zip test-pkgs/pkg-test1-no-checksum.zip"

   (define (modify-test1-no-checksum!)
     (shelly-case
      "modify test1-no-checksum"
      (define dir (make-temporary-directory))
      (define src-zip (path->complete-path "test-pkgs/pkg-test1.zip"))
      (define dest-zip (path->complete-path "test-pkgs/pkg-test1-no-checksum.zip"))
      (parameterize ([current-directory dir])
        (unzip src-zip))
      (delete-file dest-zip)
      (parameterize ([current-directory dir])
        (call-with-output-file
         "pkg-test1/main.rkt"
         #:exists 'truncate
         (lambda (o)
           (display "#lang racket/base\n(exit 17)\n" o)))
        (apply zip dest-zip (directory-list)))))

   (define (restore-test1-no-checksum!)
     (shelly-case
      "restore test1-no-checksum"
      $ "cp -f test-pkgs/pkg-test1.zip test-pkgs/pkg-test1-no-checksum.zip"))

   (shelly-install* "checksums are ignored if missing by default (local)"
                    "test-pkgs/pkg-test1-no-checksum.zip"
                    "pkg-test1-no-checksum")

   (with-fake-root
    (shelly-case
     "checksums are checked (remote, indexed)"
     (hash-set!
      *index-ht-1* "pkg-test1"
      (hasheq 'checksum
              (file->string "test-pkgs/pkg-test1-bad-checksum.zip.CHECKSUM")
              'source
              "http://localhost:9997/pkg-test1-bad-checksum.zip"))
     $ "raco pkg config --set catalogs http://localhost:9990 http://localhost:9991"
     $ "racket -e '(require pkg-test1)'" =exit> 1
     $ "raco pkg install pkg-test1" =exit> 1
     $ "racket -e '(require pkg-test1)'" =exit> 1))

   (with-fake-root
    (shelly-case
     "checksums are checked (remote)"
     $ "racket -e '(require pkg-test1)'" =exit> 1
     $ "raco pkg install http://localhost:9997/pkg-test1-bad-checksum.zip" =exit> 1
     $ "racket -e '(require pkg-test1)'" =exit> 1))

   (define (install-remote-no-checksum)
     (shelly-install*
      (format "checksums are not required remotely (remote ~a etags)"
              (if (current-serve-etags) "with" "without"))
      "http://localhost:9997/pkg-test1-no-checksum.zip"
      "pkg-test1-no-checksum"
      $ "raco pkg update pkg-test1-no-checksum"
      $ "racket -e '(require pkg-test1)'" =exit> 0
      (modify-test1-no-checksum!)
      $ "raco pkg update pkg-test1-no-checksum"
      $ "racket -e '(require pkg-test1)'" =exit> 17
      (restore-test1-no-checksum!)
      $ "raco pkg update pkg-test1-no-checksum"
      $ "racket -e '(require pkg-test1)'" =exit> 0))
   (define prev-not-modifieds server-not-modifieds)
   (install-remote-no-checksum)
   (unless (server-not-modifieds . > . prev-not-modifieds)
     (error "etag-based cache not working?"))

   (parameterize ([current-serve-etags #f])
     (install-remote-no-checksum))

   (shelly-install* "but, bad checksums can be ignored (local)"
                    "--ignore-checksums test-pkgs/pkg-test1-bad-checksum.zip"
                    "pkg-test1-bad-checksum")
   (shelly-install* "but, bad checksums can be ignored (remote)"
                    "--ignore-checksums http://localhost:9997/pkg-test1-bad-checksum.zip"
                    "pkg-test1-bad-checksum")
   (shelly-install* "but, checksums can be missing if ignored (remote)"
                    "--ignore-checksums http://localhost:9997/pkg-test1-no-checksum.zip"
                    "pkg-test1-no-checksum"))))
