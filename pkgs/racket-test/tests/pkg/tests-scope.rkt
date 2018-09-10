#lang racket/base
(require "shelly.rkt"
         "util.rkt"
         pkg/lib
         setup/dirs
         racket/format
         racket/file)

(this-test-is-run-by-the-main-test)

(pkg-tests
 (with-fake-installation
  (with-fake-root

   (shelly-case
    "default scope in different scopes"

    (define main-dir (simplify-path (build-path (find-pkgs-dir) 'up)))
    $ "racket -l racket/base -l pkg/lib -l setup/dirs -e '(cons (find-pkgs-dir) (get-all-pkg-scopes))'"
    =stdout> (string-append (~v (list (build-path (fake-installation-dir) "pkgs")
                                      (build-path main-dir "pkgs")
                                      'installation
                                      'user))
                            "\n")
    $ "raco pkg show"
    $ "raco pkg config --set -i default-scope installation"
    $ "raco pkg config -u default-scope" =stdout> "installation\n" ; inherited
    $ "raco pkg config --set default-scope user" ; set user at installation scope
    $ "raco pkg config -u default-scope" =stdout> "user\n" ; inherited
    $ "raco pkg config -i default-scope" =stdout> "user\n"
    
    $ "raco pkg config --set default-scope installation" ; set installation at user scope
    $ "raco pkg config -u default-scope" =stdout> "installation\n"
    $ "raco pkg config -i default-scope" =stdout> "user\n"
    $ "raco pkg config default-scope" =stdout> "user\n" ; misleading!

    $ "raco pkg config --set default-scope user" ; still sets user in installation
    $ "raco pkg config -i default-scope" =stdout> "user\n"
    $ "raco pkg config -u default-scope" =stdout> "installation\n"

    $ "raco pkg config -u --set default-scope user"
    $ "raco pkg config default-scope" =stdout> "user\n")

   (shelly-case
    "conflict due to installations in different scopes: installation-wide first"
    $ "raco pkg install -i --copy test-pkgs/pkg-test1"
    $ "racket -l pkg-test1"  =stdout> #rx"main loaded"
    $ "raco pkg install -u --copy test-pkgs/pkg-test1" =exit> 1 =stderr> #rx"installed in a wider scope"
    $ "raco pkg install -u --name base --copy test-pkgs/pkg-test1" =exit> 1 =stderr> #rx"installed in a wider scope"
    $ "raco pkg remove pkg-test1" =stdout> #rx"Inferred package scope: installation")

   (shelly-case
    "no conflict due to installations in different scopes: user-specific first"
    $ "raco pkg install -u --copy test-pkgs/pkg-test1"
    $ "racket -l pkg-test1"  =stdout> #rx"main loaded"
    $ "raco pkg install -i --copy test-pkgs/pkg-test1"
    $ "raco pkg install --skip-installed -u --copy test-pkgs/pkg-test1"
    $ "raco pkg remove -i --no-trash pkg-test1" =stdout> "Removing pkg-test1\n"
    $ "raco pkg remove --no-trash pkg-test1" =stdout> "Removing pkg-test1\n")

   (shelly-case
    "override conflist, installation first"
    $ "raco pkg install -i --copy test-pkgs/pkg-test1"
    $ "racket -l racket/base -l pkg-test1/number -e '(number)'"  =stdout> "1\n"
    $ "raco pkg install -u --name pkg-test1 --copy test-pkgs/pkg-test1-v2" =exit> 1
    $ "raco pkg install --force -u --name pkg-test1 --copy test-pkgs/pkg-test1-v2"
    $ "racket -l racket/base -l pkg-test1/number -e '(number)'"  =stdout> "2\n"
    $ "raco pkg remove --no-trash pkg-test1" =stdout> "Removing pkg-test1\n"
    $ "racket -l racket/base -l pkg-test1/number -e '(number)'"  =stdout> "1\n"
    $ "raco pkg remove pkg-test1" =stdout> #rx"Inferred package scope: installation")

   (shelly-case
    "check usability of user first, then installation"
    $ "raco pkg install -u --copy test-pkgs/pkg-test1"
    $ "racket -l racket/base -l pkg-test1/number -e '(number)'"  =stdout> "1\n"
    $ "raco pkg install -i --name pkg-test1 --copy test-pkgs/pkg-test1-v2"
    $ "racket -l racket/base -l pkg-test1/number -e '(number)'"  =stdout> "1\n"
    $ "raco pkg remove --no-trash pkg-test1" =stdout> "Removing pkg-test1\n"
    $ "racket -l racket/base -l pkg-test1/number -e '(number)'"  =stdout> "2\n"
    $ "raco pkg remove pkg-test1" =stdout> #rx"Inferred package scope: installation")

   (initialize-catalogs)
   (define alone-dir (make-temporary-file "alone~a" 'directory))
   (define in-search-dir (make-temporary-file "in-search~a" 'directory))
   (shelly-case
    "directory as a package scope"
    $ "raco pkg config -i --set catalogs http://localhost:9990 http://localhost:9991"
    $ "raco pkg install -i pkg-test1"
    $ "racket -l pkg-test1"  =stdout> #rx"main loaded"
    ;; won't find "base" on catalog:
    $ (~a "raco pkg install --scope-dir "alone-dir" --auto pkg-test2") =exit> 1 =stderr> #rx"cannot find package.*base"
    $ "racket -l pkg-test2"   =exit> 1 =stderr> #rx"collection not found"
    $ (~a "racket -l tests/pkg/test-scope-add "in-search-dir)
    ;; will install "pkg-test2" without an extra "pkg-test1" or "base":
    $ (~a "raco pkg install --scope-dir "in-search-dir" --auto pkg-test2")
    $ "racket -l pkg-test2"   =stdout> #rx"pkg-test2/main loaded")
   (delete-directory/files alone-dir)
   (delete-directory/files in-search-dir)

    )))
