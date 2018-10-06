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
         racket/format
         file/zip
         file/unzip
         net/url
         setup/dirs
         "shelly.rkt"
         "util.rkt")

(this-test-is-run-by-the-main-test)

(pkg-tests
 (shelly-begin
  (initialize-catalogs)

  (define-syntax-rule (shelly-install-dry-run what src)
    (shelly-case
     (format "Test dry-run installation of ~a" what)
     $ "racket -e '(require pkg-test1)'" =exit> 1
     $ (~a "raco pkg install --dry-run " src)
     $ "racket -e '(require pkg-test1)'" =exit> 1))

  (define-syntax-rule (shelly-install/d what src)
    (begin
      (shelly-install-dry-run what src)
      (shelly-install what src)))

  (define-syntax-rule (shelly-install*/d what srcs pkgs)
    (begin
      (shelly-install-dry-run what srcs)
      (shelly-install* what srcs pkgs)))

  (shelly-case
   "raco pkg install tests"
   (shelly-install/d "local package (tgz)" "test-pkgs/pkg-test1.tgz")
   (shelly-install/d "local package (zip)" "test-pkgs/pkg-test1.zip")
   (shelly-install/d "local package (file://zip)" (url->string (path->url (path->complete-path "test-pkgs/pkg-test1.zip"))))
   (shelly-install/d "local package (plt)" "test-pkgs/pkg-test1.plt")
   (shelly-install*/d "local package (zip, compiled)" "test-pkgs/pkg-test1b.zip" "pkg-test1b")
   (shelly-install*/d "local package (zip, single-collection)" 
                      "test-pkgs/pkg-test1.zip test-pkgs/pkg-test3.zip" 
                      "pkg-test1 pkg-test3")
   (shelly-install/d "local package (dir)" (url->string (path->url (path->complete-path "test-pkgs/pkg-test1"))))
   (shelly-install/d "local package (file://dir)" (url->string (path->url (path->complete-path "test-pkgs/pkg-test1"))))

   ;; Check ".zip" file with extra directory layer:
   (let ([dir (make-temporary-file "zip~a" 'directory)]
         [orig-dir (current-directory)])
     (define-values (base name dir?) (split-path dir))
     (parameterize ([current-directory base])
       (parameterize ([current-directory name])
         (unzip (build-path orig-dir "test-pkgs/pkg-test1.zip")))
       (zip (build-path dir "pkg-test1.zip") name))
     (shelly-install "local package (zip, extra layer)"
                     (build-path dir "pkg-test1.zip"))
     (delete-directory/files dir))

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
                   "http://localhost:9997/pkg-test1.tgz")
   (shelly-install "remote/URL/http package (directory)"
                   "http://localhost:9997/pkg-test1/")

   (with-fake-root
    (shelly-begin
     $ "raco pkg config --set catalogs http://localhost:9990 http://localhost:9991"
     (shelly-case
      "fails due to unrecognized scheme"
      $ "raco pkg install magic://download" =exit> 1)
     (shelly-case
      "fails due to 401 status result"
      $ "raco pkg install broken" =exit> 1 =stderr> #rx"401")
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
    $ "raco pkg install http://localhost:9997/pkg-test1.rar" =exit> 1)
   (shelly-case
    "remote/URL/http directory, no manifest fail"
    $ "raco pkg install http://localhost:9997/pkg-test1/pkg-test1/"
    =exit> 1
    =stderr> #rx"could not find MANIFEST")
   (shelly-case
    "remote/URL/http directory, bad manifest"
    ;; XXX why does this error now?
    $ "raco pkg install http://localhost:9997/pkg-test1-manifest-error/" =exit> 1)
   (shelly-case
    "remote/URL/file, bad checksum"
    $ "raco pkg install --checksum zzz http://localhost:9997/pkg-test1.tgz"
    =exit> 1
    =stderr> #rx"mismatched checksum")

   (shelly-case
    "local directory fails when not there"
    $ "raco pkg install --copy test-pkgs/pkg-test1-not-there/" =exit> 1)

   (parameterize ([current-directory test-source-directory])
     (shelly-case
      "directory fails due to path overlap"
      $ "raco pkg install test-pkgs/pkg-test1"
      =exit> 1
      =stderr> #rx"overlap"
      $ (~a "raco pkg install " (find-collects-dir))
      =exit> 1
      =stderr> #rx"overlap.*collection"
      $ (~a "raco pkg install " (collection-path "tests"))
      =exit> 1
      =stderr> #rx"overlap.*package"))

   (define tmp-dir (path->directory-path (make-temporary-file "pkg~a" 'directory)))
   $ (~a "cp -r test-pkgs/pkg-test1 "tmp-dir"pkg-test1")
   $ (~a "cp -r test-pkgs/pkg-test3 "tmp-dir"pkg-test3")

   (shelly-case
    "directory fails due to path overlap"
    $ (~a "raco pkg install "tmp-dir" "tmp-dir"pkg-test1")
    =exit> 1
    =stderr> #rx"overlap")

   (shelly-install "local package (directory)" "test-pkgs/pkg-test1/"
                   $ "racket -e '(require pkg-test1)'")
   (shelly-install* "local package (single-collection directory)" 
                    (~a tmp-dir"pkg-test1/ "tmp-dir"pkg-test3/")
                    "pkg-test1 pkg-test3"
                    $ "racket -e '(require pkg-test3)'")
   
   (shelly-install "redundant packages ignored"
                   (~a tmp-dir"pkg-test1/ "tmp-dir"pkg-test1/")
                   $ "racket -e '(require pkg-test1)'")
   
   (shelly-install "already-installed error before no-such-file error"
                   "test-pkgs/pkg-test1/"
                   $ "raco pkg install no-such-dir/pkg-test1.zip"
                   =exit> 1
                   =stderr> #rx"already installed")

   (shelly-case
    "conflicting package names disallowed"
    $ "raco pkg install --copy test-pkgs/pkg-test1/ test-pkgs/pkg-test1.zip" =exit> 1)

   $ (~a "cp -r "tmp-dir"pkg-test1 "tmp-dir"pkg-test1-linking")
   $ (~a "cp -r test-pkgs/pkg-test1-staging "tmp-dir"pkg-test1-staging")

   (parameterize ([current-directory test-source-directory])
     (with-fake-root
      (shelly-case
       "linking local directory"
       $ "racket -e '(require pkg-test1)'" =exit> 1
       $ (~a "raco pkg install --link "tmp-dir"pkg-test1-linking")
       $ "racket -e '(require pkg-test1)'"
       $ "racket -e '(require pkg-test1/a)'" =exit> 1
       $ (~a "racket -e '(require pkg/lib)' -e '(path->pkg \""tmp-dir"pkg-test1-linking\")'") =stdout> "\"pkg-test1-linking\"\n"
       $ (~a "racket -e '(require pkg/lib)' -e '(path->pkg \""tmp-dir"pkg-test1-linking/README\")'") =stdout> "\"pkg-test1-linking\"\n"
       $ "racket -e '(require pkg/lib)' -e '(path->pkg \"test-pkgs\")'" =stdout> "\"racket-test\"\n"
       $ "racket -e '(require pkg/lib)' -e '(path->pkg (collection-file-path \"main.rkt\" \"racket\"))'" =stdout> "#f\n"
       $ (~a "cp "tmp-dir"pkg-test1-staging/a.rkt "tmp-dir"pkg-test1-linking/pkg-test1/a.rkt")
       $ "racket -e '(require pkg-test1/a)'"
       $ (~a "rm -f "tmp-dir"pkg-test1-linking/pkg-test1/a.rkt")
       $ "racket -e '(require pkg-test1/a)'" =exit> 1
       $ "raco pkg remove pkg-test1-linking"
       $ "racket -e '(require pkg-test1)'" =exit> 1)))

   $ (~a "cp -r "tmp-dir"pkg-test3 "tmp-dir"pkg-test3-linking")

   (with-fake-root
    (shelly-case
     "linking local directory, single-collection"
     $ "racket -e '(require pkg-test3)'" =exit> 1
     $ (~a "raco pkg install --link "tmp-dir"pkg-test1 "tmp-dir"pkg-test3-linking")
     $ "racket -e '(require pkg-test3-linking)'"
     $ "racket -e '(require pkg-test3)'" =exit> 1
     $ "raco pkg remove pkg-test1 pkg-test3-linking"
     $ "racket -e '(require pkg-test3-linking)'" =exit> 1))

   (delete-directory/files tmp-dir)

   (with-fake-root
    (shelly-case
     "remote/name package, doesn't work when no package there"
     $ "raco pkg config --set catalogs http://localhost:9990"
     $ "raco pkg install pkg-test1-not-there" =exit> 1))

   (with-fake-root
    (shelly-case
     "implicitly single-collection"
     $ "racket -e '(require pkg-c/c)'" =exit> 1
     $ "raco pkg install --copy test-pkgs/pkg-c"
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
     $ "racket -e '(require pkg-test1)'" =exit> 1))

   (with-fake-root
    (shelly-case
     "git package that requires authentication"
     $ "raco pkg config --set catalogs http://localhost:9990"
     $ "raco pkg install pkg-git" =exit> 1
     $ "raco pkg config --set git-checkout-credentials user:bad-password"
     $ "raco pkg install pkg-git" =exit> 1
     $ "raco pkg config --set git-checkout-credentials user:password"
     $ "raco pkg install pkg-git"))

   (parameterize ([current-directory test-source-directory])
     (with-fake-root
         (shelly-case
          "install package in default mode using local directory catalog specified via pkg config --set catalogs"
          (define tmp-dir (path->directory-path (make-temporary-file "tmp~a" 'directory)))
          (define catalog-dir (build-path tmp-dir "catalog"))
          (make-directory catalog-dir)
          (define pkgs-dir (build-path tmp-dir "pkgs"))
          (make-directory pkgs-dir)
          (copy-directory/files (build-path "test-pkgs" "pkg-strip") (build-path pkgs-dir "pkg-strip"))
          $ (format "racket -l pkg/dirs-catalog ~a ~a" catalog-dir pkgs-dir)
          $ "raco pkg install pkg-strip" =exit> 1
          $ (format "raco pkg config --set catalogs file:///~a" (path->string catalog-dir))
          $ "raco pkg install pkg-strip"
          $ "racket -l pkg-strip" =stdout> "this pkg can be stripped in multiple modes\n"
          (delete-directory/files tmp-dir)))

     (with-fake-root
         (shelly-case
          "install package in default mode using local directory catalog specified via pkg config --set catalogs"
          (define tmp-dir (path->directory-path (make-temporary-file "tmp~a" 'directory)))
          (define catalog-dir (build-path tmp-dir "catalog"))
          (make-directory catalog-dir)
          (define pkgs-dir (build-path tmp-dir "pkgs"))
          (make-directory pkgs-dir)
          (copy-directory/files (build-path "test-pkgs" "pkg-strip") (build-path pkgs-dir "pkg-strip"))
          $ (format "racket -l pkg/dirs-catalog ~a ~a" catalog-dir pkgs-dir)
          $ "raco pkg install pkg-strip" =exit> 1
          $ (format "raco pkg config --set catalogs file:///~a" (path->string catalog-dir))
          $ "raco pkg install pkg-strip"
          $ "racket -l pkg-strip" =stdout> "this pkg can be stripped in multiple modes\n"
          (delete-directory/files tmp-dir)))

     (with-fake-root
         (shelly-case
          "install package in source mode using local directory catalog specified via pkg config --set catalogs"
          (define tmp-dir (path->directory-path (make-temporary-file "tmp~a" 'directory)))
          (define catalog-dir (build-path tmp-dir "catalog"))
          (make-directory catalog-dir)
          (define pkgs-dir (build-path tmp-dir "pkgs"))
          (make-directory pkgs-dir)
          (copy-directory/files (build-path "test-pkgs" "pkg-strip") (build-path pkgs-dir "pkg-strip"))
          $ (format "racket -l pkg/dirs-catalog ~a ~a" catalog-dir pkgs-dir)
          $ "raco pkg install --source pkg-strip" =exit> 1
          $ (format "raco pkg config --set catalogs file:///~a" (path->string catalog-dir))
          $ "raco pkg install --source pkg-strip"
          $ "racket -l pkg-strip" =stdout> "this pkg can be stripped in multiple modes\n"
          (delete-directory/files tmp-dir)))

     (with-fake-root
         (shelly-case
          "install package in source mode using local directory catalog specified via pkg install --catalog"
          (define tmp-dir (path->directory-path (make-temporary-file "tmp~a" 'directory)))
          (define catalog-dir (build-path tmp-dir "catalog"))
          (make-directory catalog-dir)
          (define pkgs-dir (build-path tmp-dir "pkgs"))
          (make-directory pkgs-dir)
          (copy-directory/files (build-path "test-pkgs" "pkg-strip") (build-path pkgs-dir "pkg-strip"))
          $ (format "racket -l pkg/dirs-catalog ~a ~a" catalog-dir pkgs-dir)
          $ "raco pkg install --source pkg-strip" =exit> 1
          $ (format "raco pkg install --source --catalog file:///~a pkg-strip" (path->string catalog-dir))
          $ "racket -l pkg-strip" =stdout> "this pkg can be stripped in multiple modes\n"
          (delete-directory/files tmp-dir)))

     (with-fake-root
         (shelly-case
          "install package in binary mode using local directory catalog specified via pkg config --set catalogs"
          (define pkg-strip-orig-dir (build-path "test-pkgs" "pkg-strip"))
          (define tmp-dir (path->directory-path (make-temporary-file "tmp~a" 'directory)))
          (define catalog-dir (build-path tmp-dir "catalog"))
          (make-directory catalog-dir)
          (define pkgs-dir (build-path tmp-dir "pkgs"))
          (make-directory pkgs-dir)
          (define pkg-strip-dir (build-path tmp-dir "pkg-strip"))
          (define pkg-strip-binary-dir (build-path pkgs-dir "pkg-strip"))
          (make-directory pkg-strip-binary-dir)
          (copy-directory/files pkg-strip-orig-dir pkg-strip-dir)
          (parameterize ([current-directory pkg-strip-dir])
            (shelly-begin
             $ "raco make info.rkt main.rkt"))
          $ (format "racket -e '(require pkg/strip) (generate-stripped-directory (quote binary) ~s ~s)'"
                    (path->string pkg-strip-dir)
                    (path->string pkg-strip-binary-dir))
          $ (format "racket -l pkg/dirs-catalog ~a ~a" catalog-dir pkgs-dir)
          $ "raco pkg install --binary pkg-strip" =exit> 1
          $ (format "raco pkg config --set catalogs file:///~a" (path->string catalog-dir))
          $ "raco pkg install --binary pkg-strip"
          $ "racket -l pkg-strip" =stdout> "this pkg can be stripped in multiple modes\n"
          (delete-directory/files tmp-dir)))

     (with-fake-root
         (shelly-case
          "install package in binary mode using local directory catalog specified via pkg install --catalog"
          (define pkg-strip-orig-dir (build-path "test-pkgs" "pkg-strip"))
          (define tmp-dir (path->directory-path (make-temporary-file "tmp~a" 'directory)))
          (define catalog-dir (build-path tmp-dir "catalog"))
          (make-directory catalog-dir)
          (define pkgs-dir (build-path tmp-dir "pkgs"))
          (make-directory pkgs-dir)
          (define pkg-strip-dir (build-path tmp-dir "pkg-strip"))
          (define pkg-strip-binary-dir (build-path pkgs-dir "pkg-strip"))
          (make-directory pkg-strip-binary-dir)
          (copy-directory/files pkg-strip-orig-dir pkg-strip-dir)
          (parameterize ([current-directory pkg-strip-dir])
            (shelly-begin
             $ "raco make info.rkt main.rkt"))
          $ (format "racket -e '(require pkg/strip) (generate-stripped-directory (quote binary) ~s ~s)'"
                    (path->string pkg-strip-dir)
                    (path->string pkg-strip-binary-dir))
          $ (format "racket -l pkg/dirs-catalog ~a ~a" catalog-dir pkgs-dir)
          $ "raco pkg install --binary pkg-strip" =exit> 1
          $ (format "raco pkg install --catalog file:///~a --binary pkg-strip" (path->string catalog-dir))
          $ "racket -l pkg-strip" =stdout> "this pkg can be stripped in multiple modes\n"
          (delete-directory/files tmp-dir)))

     (with-fake-root
         (shelly-case
          "install package in binary-lib mode using local directory catalog specified via pkg config --set catalogs"
          (define pkg-strip-orig-dir (build-path "test-pkgs" "pkg-strip"))
          (define tmp-dir (path->directory-path (make-temporary-file "tmp~a" 'directory)))
          (define catalog-dir (build-path tmp-dir "catalog"))
          (make-directory catalog-dir)
          (define pkgs-dir (build-path tmp-dir "pkgs"))
          (make-directory pkgs-dir)
          (define pkg-strip-dir (build-path tmp-dir "pkg-strip"))
          (define pkg-strip-binary-lib-dir (build-path pkgs-dir "pkg-strip"))
          (make-directory pkg-strip-binary-lib-dir)
          (copy-directory/files pkg-strip-orig-dir pkg-strip-dir)
          (parameterize ([current-directory pkg-strip-dir])
            (shelly-begin
             $ "raco make info.rkt main.rkt"))
          $ (format "racket -e '(require pkg/strip) (generate-stripped-directory (quote binary-lib) ~s ~s)'"
                    (path->string pkg-strip-dir)
                    (path->string pkg-strip-binary-lib-dir))
          $ (format "racket -l pkg/dirs-catalog ~a ~a" catalog-dir pkgs-dir)
          $ "raco pkg install --binary-lib pkg-strip" =exit> 1
          $ (format "raco pkg config --set catalogs file:///~a" (path->string catalog-dir))
          $ "raco pkg install --binary-lib pkg-strip"
          $ "racket -l pkg-strip" =stdout> "this pkg can be stripped in multiple modes\n"
          (delete-directory/files tmp-dir)))

     (with-fake-root
         (shelly-case
          "install package in binary-lib mode using local directory catalog specified via pkg install --catalog"
          (define pkg-strip-orig-dir (build-path "test-pkgs" "pkg-strip"))
          (define tmp-dir (path->directory-path (make-temporary-file "tmp~a" 'directory)))
          (define catalog-dir (build-path tmp-dir "catalog"))
          (make-directory catalog-dir)
          (define pkgs-dir (build-path tmp-dir "pkgs"))
          (make-directory pkgs-dir)
          (define pkg-strip-dir (build-path tmp-dir "pkg-strip"))
          (define pkg-strip-binary-lib-dir (build-path pkgs-dir "pkg-strip"))
          (make-directory pkg-strip-binary-lib-dir)
          (copy-directory/files pkg-strip-orig-dir pkg-strip-dir)
          (parameterize ([current-directory pkg-strip-dir])
            (shelly-begin
             $ "raco make info.rkt main.rkt"))
          $ (format "racket -e '(require pkg/strip) (generate-stripped-directory (quote binary-lib) ~s ~s)'"
                    (path->string pkg-strip-dir)
                    (path->string pkg-strip-binary-lib-dir))
          $ (format "racket -l pkg/dirs-catalog ~a ~a" catalog-dir pkgs-dir)
          $ "raco pkg install --binary-lib pkg-strip" =exit> 1
          $ (format "raco pkg install --catalog file:///~a --binary-lib pkg-strip" (path->string catalog-dir))
          $ "racket -l pkg-strip" =stdout> "this pkg can be stripped in multiple modes\n"
          (delete-directory/files tmp-dir)))))))
