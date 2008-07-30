;; require this file to run all of the test suites for redex.

(module run-tests mzscheme
  (require "pict-test.ss" ;; this one should go last, so it is listed first
           
           "bitmap-test.ss" ;; second to last
           
           "core-layout-test.ss"
           
           "rg-test.ss"
           "term-test.ss"
           "tl-test.ss"
           "matcher-test.ss"
           "lw-test.ss")
  
  (printf "\nWARNING: didn't run color-test.ss or subst-test.ss\n"))

