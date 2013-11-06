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
   "raco pkg install tests"
   (shelly-install "local package (tgz)" "test-pkgs/pkg-test1.tgz")
   (shelly-install "local package (zip)" "test-pkgs/pkg-test1.zip")
   (shelly-install "local package (plt)" "test-pkgs/pkg-test1.plt")
   (shelly-install* "local package (zip, compiled)" "test-pkgs/pkg-test1b.zip" "pkg-test1b")
   (shelly-install* "local package (zip, single-collection)" 
                    "test-pkgs/pkg-test1.zip test-pkgs/pkg-test3.zip" 
                    "pkg-test1 pkg-test3")

   (with-fake-root
    (shelly-case
     "local package (old plt)" 
     $ "raco pkg install test-pkgs/pkg-test-ancient.plt"
     $ "racket -l test-pkg-ancient" =stdout> "'yea\n"
     $ "raco pkg remove pkg-test-ancient"))

   (shelly-case
    "invalid package format is an error"
    $ "raco pkg install test-pkgs/pkg-test1.zip.CHECKSUM" =exit> 1)

   (shelly-install "remote/URL/http package (file, tgz)"
                   "http://localhost:9999/pkg-test1.tgz")
   (shelly-install "remote/URL/http package (directory)"
                   "http://localhost:9999/pkg-test1/")

   (with-fake-root
    (shelly-begin
     $ "raco pkg config --set catalogs http://localhost:9990 http://localhost:9991"
     (shelly-case
      "fails due to unrecognized scheme"
      $ "raco pkg install magic://download" =exit> 1)
     (shelly-case
      "local directory name fails because not inferred as such (inferred as package name)"
      $ "raco pkg install test-pkgs" =exit> 1)
     (shelly-case
      "local file name with bad suffix and not a package name or directory"
      $ "raco pkg install tests-install.rkt" =exit> 1)
     (shelly-case
      "not a valid (inferred) package name"
      $ "raco pkg install 1+2" =exit> 1)))

   (shelly-case
    "local file fails because called a directory"
    $ "raco pkg install --type dir test-pkgs/pkg-a-first.plt" =exit> 1)
   (shelly-case
    "local directory name fails because called a file"
    $ "raco pkg install --type file test-pkgs/pkg-a-first/" =exit> 1)
   (shelly-case
    "local directory name fails because called a URL"
    $ "raco pkg install --type file-url test-pkgs/pkg-a-first/" =exit> 1)
   (shelly-case
    "local file fails due to mismatch with specified checksum"
    $ "raco pkg install --checksum zzz test-pkgs/pkg-a-first.plt"
    =exit> 1
    =stderr> #rx"unexpected checksum")

   (shelly-case
    "remote/URL/http directory, non-existant file"
    $ "raco pkg install http://localhost:9999/pkg-test1.rar" =exit> 1)
   (shelly-case
    "remote/URL/http directory, no manifest fail"
    $ "raco pkg install http://localhost:9999/pkg-test1/pkg-test1/"
    =exit> 1
    =stderr> #rx"could not find MANIFEST")
   (shelly-case
    "remote/URL/http directory, bad manifest"
    ;; XXX why does this error now?
    $ "raco pkg install http://localhost:9999/pkg-test1-manifest-error/" =exit> 1)
   (shelly-case
    "remote/URL/file, bad checksum"
    $ "raco pkg install --checksum zzz http://localhost:9999/pkg-test1.tgz"
    =exit> 1
    =stderr> #rx"mismatched checksum")

   (shelly-case
    "local directory fails when not there"
    $ "raco pkg install test-pkgs/pkg-test1-not-there/" =exit> 1)

   (shelly-install "local package (directory)" "test-pkgs/pkg-test1/"
                   $ "racket -e '(require pkg-test1)'")
   (shelly-install* "local package (single-collection directory)" 
                    "test-pkgs/pkg-test1/ test-pkgs/pkg-test3/"
                    "pkg-test1 pkg-test3"
                    $ "racket -e '(require pkg-test3)'")

   (shelly-install "redundant packages ignored" "test-pkgs/pkg-test1/ test-pkgs/pkg-test1/"
                   $ "racket -e '(require pkg-test1)'")
   (shelly-case
    "conflicting package names disallowed"
    $ "raco pkg install  test-pkgs/pkg-test1/ test-pkgs/pkg-test1.zip" =exit> 1)

   (with-fake-root
    (shelly-case
     "linking local directory"
     (shelly-wind
      $ "cp -r test-pkgs/pkg-test1 test-pkgs/pkg-test1-linking"
      $ "racket -e '(require pkg-test1)'" =exit> 1
      $ "raco pkg install --link test-pkgs/pkg-test1-linking"
      $ "racket -e '(require pkg-test1)'"
      $ "racket -e '(require pkg-test1/a)'" =exit> 1
      $ "racket -e '(require pkg/lib)' -e '(path->pkg \"test-pkgs/pkg-test1-linking\")'" =stdout> "\"pkg-test1-linking\"\n"
      $ "racket -e '(require pkg/lib)' -e '(path->pkg \"test-pkgs/pkg-test1-linking/README\")'" =stdout> "\"pkg-test1-linking\"\n"
      $ "racket -e '(require pkg/lib)' -e '(path->pkg \"test-pkgs\")'" =stdout> "\"racket-test\"\n"
      $ "racket -e '(require pkg/lib)' -e '(path->pkg (collection-file-path \"main.rkt\" \"racket\"))'" =stdout> "#f\n"
      $ "cp test-pkgs/pkg-test1-staging/a.rkt test-pkgs/pkg-test1-linking/pkg-test1/a.rkt"
      $ "racket -e '(require pkg-test1/a)'"
      $ "rm -f test-pkgs/pkg-test1-linking/pkg-test1/a.rkt"
      $ "racket -e '(require pkg-test1/a)'" =exit> 1
      $ "raco pkg remove pkg-test1-linking"
      $ "racket -e '(require pkg-test1)'" =exit> 1
      (finally
       $ "rm -r test-pkgs/pkg-test1-linking"))))

   (with-fake-root
    (shelly-case
     "linking local directory, single-collection"
     (shelly-wind
      $ "cp -r test-pkgs/pkg-test3 test-pkgs/pkg-test3-linking"
      $ "racket -e '(require pkg-test3)'" =exit> 1
      $ "raco pkg install --link test-pkgs/pkg-test1 test-pkgs/pkg-test3-linking"
      $ "racket -e '(require pkg-test3-linking)'"
      $ "racket -e '(require pkg-test3)'" =exit> 1
      $ "raco pkg remove pkg-test1 pkg-test3-linking"
      $ "racket -e '(require pkg-test3-linking)'" =exit> 1
      (finally
       $ "rm -r test-pkgs/pkg-test3-linking"))))

   (with-fake-root
    (shelly-case
     "remote/name package, doesn't work when no package there"
     $ "raco pkg config --set catalogs http://localhost:9990"
     $ "raco pkg install pkg-test1-not-there" =exit> 1))

   (with-fake-root
    (shelly-case
     "implicitly single-collection"
     $ "racket -e '(require pkg-c/c)'" =exit> 1
     $ "raco pkg install --link test-pkgs/pkg-c"
     $ "racket -e '(require pkg-c/c)'" =stdout> "'c\n"))

   (with-fake-root
    (shelly-case
     "remote/name package"
     $ "raco pkg config --set catalogs http://localhost:9990"
     $ "racket -e '(require pkg-test1)'" =exit> 1
     $ "raco pkg install pkg-test1"
     $ "racket -e '(require pkg-test1)'"
     $ "raco pkg remove pkg-test1"
     $ "racket -e '(require pkg-test1)'" =exit> 1))

   (with-fake-root
    (shelly-case
     "remote/name package (multi)"
     $ "raco pkg config --set catalogs http://localhost:9990 http://localhost:9991"
     $ "racket -e '(require pkg-test1)'" =exit> 1
     $ "raco pkg install --deps search-auto pkg-test2-snd"
     $ "racket -e '(require pkg-test1)'"
     $ "racket -e '(require pkg-test2)'"
     $ "racket -e '(require pkg/lib)' -e '(path->pkg (pkg-directory \"pkg-test1\"))'" =stdout> "\"pkg-test1\"\n"
     $ "racket -e '(require pkg/lib)' -e '(path->pkg (build-path (pkg-directory \"pkg-test1\") \"pkg-test2\"))'"
     =stdout> "\"pkg-test1\"\n"
     $ "raco pkg remove pkg-test2-snd pkg-test1"
     $ "racket -e '(require pkg-test1)'" =exit> 1)))))
