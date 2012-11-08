#lang racket/base
(require "shelly.rkt"
         "util.rkt")

(pkg-tests
 (with-fake-root
  (shelly-case
   "The installation directory is not touched until a package can definitely be installed AND one fail reverts the whole install"

   ;; Step 1. Try to install a package that will fail
   $ "raco pkg install test-pkgs/planet2-test1.zip test-pkgs/planet2-test1.zip" 
   =exit> 1
   =stderr> #rx"conflicts with \"planet2-test1\""

   ;; Step 2. Try to install safely
   $ "raco pkg install test-pkgs/planet2-test1.zip")))
