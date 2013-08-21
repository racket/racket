#lang racket/base
(require "shelly.rkt"
         "util.rkt")

(pkg-tests
 (with-fake-root
  (shelly-case
   "raco install/update uses raco setup, unless you turn it off (cmdline)"
   $ "raco pkg create --format plt test-pkgs/raco-pkg"
   $ "raco raco-pkg" =exit> 1
   $ "raco pkg install --no-setup test-pkgs/raco-pkg.plt"
   $ "raco raco-pkg" =exit> 1))

 (with-fake-root
  (shelly-case
   "raco install/update uses raco setup, unless you turn it off (env)"
   (putenv "PLT_PKG_NOSETUP" "1")
   $ "raco pkg create --format plt test-pkgs/raco-pkg"
   $ "raco raco-pkg" =exit> 1
   $ "raco pkg install --no-setup test-pkgs/raco-pkg.plt"
   $ "raco raco-pkg" =exit> 1))

 (with-fake-root
  (shelly-case
   "raco install/update uses raco setup"
   (putenv "PLT_PKG_NOSETUP" "")
   $ "raco pkg create --format plt test-pkgs/raco-pkg"
   $ "raco raco-pkg" =exit> 1
   $ "raco pkg install test-pkgs/raco-pkg.plt"
   $ "raco raco-pkg" =exit> 0))

 (with-fake-root
  (shelly-case
   "raco install uses raco setup with single collect"
   (putenv "PLT_PKG_NOSETUP" "")
   $ "raco pkg install --copy test-pkgs/pkg-test3-v3" =exit> 0))

 (shelly-begin
  (initialize-catalogs)
  
  (shelly-case
   "update of package runs setup on package with dependency"
   (putenv "PLT_PKG_NOSETUP" "")
   (shelly-wind
    $ "mkdir -p test-pkgs/update-test"
    $ "cp -f test-pkgs/pkg-test1.zip test-pkgs/update-test/pkg-test1.zip"
    $ "cp -f test-pkgs/pkg-test1.zip.CHECKSUM test-pkgs/update-test/pkg-test1.zip.CHECKSUM"
    (shelly-install* "remote packages can be updated"
                     "http://localhost:9999/update-test/pkg-test1.zip"
                     "pkg-test1 pkg-test3"
                     $ "raco pkg install --copy test-pkgs/pkg-test3"
                     $ "racket -l pkg-test3/number" =exit> 1
                     $ "cp -f test-pkgs/pkg-test1-v2.zip test-pkgs/update-test/pkg-test1.zip"
                     $ "cp -f test-pkgs/pkg-test1-v2.zip.CHECKSUM test-pkgs/update-test/pkg-test1.zip.CHECKSUM"
                     $ "raco pkg update pkg-test1" =exit> 0
                     $ "racket -l pkg-test3/number" =exit> 2)
    (finally
     $ "rm -f test-pkgs/update-test/pkg-test1.zip"
     $ "rm -f test-pkgs/update-test/pkg-test1.zip.CHECKSUM")))))
