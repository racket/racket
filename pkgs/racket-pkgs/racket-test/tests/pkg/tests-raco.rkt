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
   $ "raco raco-pkg" =exit> 1
   (putenv "PLT_PKG_NOSETUP" "")))

 (with-fake-root
  (shelly-case
   "raco install/update uses raco setup"
   $ "raco pkg create --format plt test-pkgs/raco-pkg"
   $ "raco raco-pkg" =exit> 1
   $ "raco pkg install test-pkgs/raco-pkg.plt"
   $ "raco raco-pkg" =exit> 0))

 (with-fake-root
  (shelly-case
   "raco install uses raco setup with single collect"
   $ "raco pkg install test-pkgs/pkg-test3-v3" =exit> 0)))

