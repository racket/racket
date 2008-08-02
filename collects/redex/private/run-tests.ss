;; require this file to run all of the test suites for redex.

#lang scheme/base
(require scheme/runtime-path)

(define test-files
  '("lw-test.ss" 
    "matcher-test.ss" 
    "tl-test.ss" 
    "term-test.ss" 
    "rg-test.ss" 
    "core-layout-test.ss" 
    "bitmap-test.ss" 
    "pict-test.ss"))

(define-runtime-path here ".")

(for-each
 (λ (test-file)
   (printf "requiring ~a\n" test-file)
   (dynamic-require (build-path here test-file) #f))
 test-files)

(printf "\nWARNING: didn't run color-test.ss or subst-test.ss\n")
