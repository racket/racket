#lang racket/base
(require "shelly.rkt"
         "util.rkt")

(pkg-tests
 (with-fake-installation
  (with-fake-root

   (shelly-case
    "default scope in different scopes"
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
    $ "raco pkg install -i test-pkgs/pkg-test1"
    $ "racket -l pkg-test1"  =stdout> #rx"main loaded"
    $ "raco pkg install -u test-pkgs/pkg-test1" =exit> 1 =stderr> #rx"installed in a wider scope"
    $ "raco pkg install -u --name base test-pkgs/pkg-test1" =exit> 1 =stderr> #rx"installed in a wider scope"
    $ "raco pkg remove pkg-test1" =stdout> #rx"Inferred package scope: installation")

   (shelly-case
    "conflict due to installations in different scopes: user-specific first"
    $ "raco pkg install -u test-pkgs/pkg-test1"
    $ "racket -l pkg-test1"  =stdout> #rx"main loaded"
    $ "raco pkg install -i test-pkgs/pkg-test1" =exit> 1 =stderr> #rx"packages in different scopes conflict"
    $ "raco pkg remove pkg-test1" =stdout> "Removing pkg-test1\n")

   (shelly-case
    "override conflist, installation first"
    $ "raco pkg install -i test-pkgs/pkg-test1"
    $ "racket -l racket/base -l pkg-test1/number -e '(number)'"  =stdout> "1\n"
    $ "raco pkg install -u --name pkg-test1 test-pkgs/pkg-test1-v2" =exit> 1
    $ "raco pkg install --force -u --name pkg-test1 test-pkgs/pkg-test1-v2"
    $ "racket -l racket/base -l pkg-test1/number -e '(number)'"  =stdout> "2\n"
    $ "raco pkg remove pkg-test1" =stdout> "Removing pkg-test1\n"
    $ "racket -l racket/base -l pkg-test1/number -e '(number)'"  =stdout> "1\n"
    $ "raco pkg remove pkg-test1" =stdout> #rx"Inferred package scope: installation")

   (shelly-case
    "override conflist, user first"
    $ "raco pkg install -u test-pkgs/pkg-test1"
    $ "racket -l racket/base -l pkg-test1/number -e '(number)'"  =stdout> "1\n"
    $ "raco pkg install -i --name pkg-test1 test-pkgs/pkg-test1-v2" =exit> 1
    $ "raco pkg install --force -i --name pkg-test1 test-pkgs/pkg-test1-v2"
    $ "racket -l racket/base -l pkg-test1/number -e '(number)'"  =stdout> "1\n"
    $ "raco pkg remove pkg-test1" =stdout> "Removing pkg-test1\n"
    $ "racket -l racket/base -l pkg-test1/number -e '(number)'"  =stdout> "2\n"
    $ "raco pkg remove pkg-test1" =stdout> #rx"Inferred package scope: installation")

    )))
