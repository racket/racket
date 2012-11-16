#lang racket/base
(require "shelly.rkt"
         "util.rkt")

(pkg-tests
 (with-fake-root
  (shelly-case
   "raco install/update uses raco setup, unless you turn it off (cmdline)"
   $ "raco pkg create --format plt test-pkgs/raco-pkg"
   $ "raco raco-pkg" =exit> 1
   $ "raco pkg install --dont-setup test-pkgs/raco-pkg.plt"
   $ "raco raco-pkg" =exit> 1))

 (with-fake-root
  (shelly-case
   "raco install/update uses raco setup, unless you turn it off (env)"
   (putenv "PLT_PLANET2_DONTSETUP" "1")
   $ "raco pkg create --format plt test-pkgs/raco-pkg"
   $ "raco raco-pkg" =exit> 1
   $ "raco pkg install --dont-setup test-pkgs/raco-pkg.plt"
   $ "raco raco-pkg" =exit> 1
   (putenv "PLT_PLANET2_DONTSETUP" "")))

 (with-fake-root
  (shelly-case
   "raco install/update uses raco setup"
   $ "raco pkg create --format plt test-pkgs/raco-pkg"
   $ "raco raco-pkg" =exit> 1
   $ "raco pkg install test-pkgs/raco-pkg.plt"
   $ "raco raco-pkg" =exit> 0)))
