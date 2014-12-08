#lang racket/base
(require rackunit
         racket/system
         racket/match
         racket/format
         racket/file
         racket/runtime-path
         racket/path
         racket/list
         "shelly.rkt"
         "util.rkt")

(this-test-is-run-by-the-main-test)

;; todo: to move the test packages to the "plt" account on GitHub

(pkg-tests
 (define (test-remote url)
   (shelly-begin
    (shelly-install "remote/git" 
                    (~a url "?path=pkg-test1")
     $ "racket -l racket/base -l pkg-test1/number -e '(number)'" =stdout> "1\n")
    (shelly-install "remote/git with slash"
                    (~a url "?path=pkg-test1/"))
    (shelly-install 
     "remote/git with tag"
     (~a url "?path=pkg-test1/#hundred")
     $ "racket -l racket/base -l pkg-test1/number -e '(number)'" =stdout> "100\n")
    (shelly-install 
     "remote/git with commit"
     (~a url "?path=pkg-test1/#f9b4eef22")
     $ "racket -l racket/base -l pkg-test1/number -e '(number)'" =stdout> "100\n")
    (shelly-install 
     "remote/git with checksum"
     (~a "--checksum f9b4eef22cdd9ab88b254cb027fc1ebe7fb596fd " url "?path=pkg-test1")
     $ "racket -l racket/base -l pkg-test1/number -e '(number)'" =stdout> "100\n"
     $ "raco pkg update pkg-test1"
     $ "racket -l racket/base -l pkg-test1/number -e '(number)'" =stdout> "1\n")

    (hash-set! *index-ht-1* "pkg-test1-git-different-checksum"
               (hasheq 'checksum
                       "f9b4eef22cdd9ab88b254cb027fc1ebe7fb596fd"
                       'source
                       (~a url "?path=pkg-test1")))

    (with-fake-root
        (shelly-case
         "remote/name package"
         $ "raco pkg config --set catalogs http://localhost:9990"
         $ "racket -l pkg-test1/number" =exit> 1
         $ "raco pkg install pkg-test1-git-different-checksum"
         $ "racket -l racket/base -l pkg-test1/number -e '(number)'" =stdout> "100\n"
         $ "raco pkg remove pkg-test1-git-different-checksum"
         $ "racket -l pkg-test1/number" =exit> 1))))

 (test-remote "git://github.com/mflatt/pkg-test")
 (test-remote "https://github.com/mflatt/pkg-test.git")
 (test-remote "https://bitbucket.org/mflatt/pkg-test.git")

 (define (try-git-repo label type+repo)
   (define tmp-dir (make-temporary-file "~a-clone" 'directory))
   (shelly-install
    label
    type+repo
    (shelly-wind
     $ "racket -l racket/base -l pkg-test1/number -e '(number)'" =stdout> "10\n"
     $ (~a "raco pkg update --clone " tmp-dir " pkg-test1")
     $ "racket -l racket/base -l pkg-test1/number -e '(number)'" =stdout> "10\n"
     $ (~a "raco pkg update " type+repo)
     (finally
      (delete-directory/files tmp-dir)))))
 
 (try-git-repo
  "remote/github with auto prefix and with branch"
  "--type github mflatt/pkg-test?path=pkg-test1/#alt")
 (try-git-repo
  "remote/git type"
  "--type git https://bitbucket.org/mflatt/pkg-test?path=pkg-test1#alt"))
