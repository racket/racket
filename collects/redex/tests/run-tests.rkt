;; require this file to run all of the test suites for redex.

#lang scheme/base
(require scheme/runtime-path
         scheme/cmdline
         scheme/match
         "test-util.ss")

(define test-bitmaps? #t)
(define test-examples? #f)

(command-line
 #:once-each
 [("--no-bitmaps") "executes bitmap-test.ss" (set! test-bitmaps? #f)]
 [("--examples") "executes the tests in the examples directory" (set! test-examples? #t)])

(define test-files
  (append
   '("lw-test.ss" 
     "matcher-test.ss" 
     "tl-test.ss" 
     "term-test.ss" 
     "rg-test.ss" 
     "keyword-macros-test.ss"
     "core-layout-test.ss" 
     "pict-test.ss"
     "hole-test.ss")
   (if test-bitmaps? '("bitmap-test.ss") '())
   (if test-examples? 
       '("../examples/pi-calculus.ss"
         ("../examples/beginner.ss" main)
         "../examples/racket-machine/reduction-test.ss"
         "../examples/racket-machine/verification-test.ss"
         "../examples/delim-cont/tests.rkt"
         ("../examples/r6rs/r6rs-tests.ss" main))
       '())))

(define-runtime-path here ".")

(define (flush)
  ;; these flushes are here for running under cygwin, 
  ;; which somehow makes mzscheme think it isn't using
  ;; an interative port
  (flush-output (current-error-port))
  (flush-output (current-output-port)))

(for-each
 (λ (test-file)
   (let-values ([(file provided action)
                 (match test-file
                   [(list (? string? file) id)
                    (values file id (λ (x) (x)))]
                   [(? string?) 
                    (values test-file #f values)])])
     (flush)
     (printf "testing ~a\n" file)
     (flush)
     (action (dynamic-require (build-path here file) provided))
     (flush)))
 test-files)

(printf "\nWARNING: didn't run color-test.ss\n")
(flush)
