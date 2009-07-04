;; require this file to run all of the test suites for redex.

#lang scheme/base
(require scheme/runtime-path)

(define test-files
  '("lw-test.ss" 
    "matcher-test.ss" 
    "tl-test.ss" 
    "term-test.ss" 
    "rg-test.ss" 
    "keyword-macros-test.ss"
    "core-layout-test.ss" 
    "bitmap-test.ss" 
    "pict-test.ss"))

(define-runtime-path here ".")

(define (flush)
  ;; these flushes are here for running under cygwin, 
  ;; which somehow makes mzscheme think it isn't using
  ;; an interative port
  (flush-output (current-error-port))
  (flush-output (current-output-port)))

(for-each
 (λ (test-file)
   (flush)
   (printf "requiring ~a\n" test-file)
   (flush)
   (dynamic-require (build-path here test-file) #f)
   (flush))
 test-files)

(printf "\nWARNING: didn't run color-test.ss or subst-test.ss\n")
(flush)
