#lang racket/base

;; require this file to run all of the test suites for redex.

(require racket/runtime-path
         racket/cmdline
         racket/match
         "test-util.rkt"
         "bitmap-test-util.rkt")

(define test-examples? #f)

(command-line
 #:once-each
 [("--no-bitmap-gui") "skips the GUI for bitmap-test.rkt" (show-bitmap-test-gui? #f)]
 [("--examples") "executes the tests in the examples directory" (set! test-examples? #t)])

(define test-files
  (append
   '("lw-test.rkt"
     "matcher-test.rkt"
     "rewrite-side-condition-test.rkt"
     "unify-tests.rkt"
     "tl-test.rkt"
     "term-test.rkt"
     "rg-test.rkt"
     "keyword-macros-test.rkt"
     "core-layout-test.rkt"
     "pict-test.rkt"
     "hole-test.rkt"
     "stepper-test.rkt"
     "defined-checks-test.rkt"
     "check-syntax-test.rkt"
     "test-docs-complete.rkt"
     "tut-subst-test.rkt"
     "bitmap-test.rkt")
   (if test-examples?
       '("../examples/cbn-letrec.rkt"
         "../examples/stlc.rkt"
         "../examples/pi-calculus.rkt"
         "../examples/list-machine/test.rkt"
         ("../examples/beginner.rkt" main)
         "../examples/racket-machine/reduction-test.rkt"
         "../examples/racket-machine/verification-test.rkt"
         "../examples/delim-cont/test.rkt"
         "../examples/cont-mark-transform/all-test.rkt"
         ("../examples/r6rs/r6rs-tests.rkt" main))
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
     (printf "running ~a\n" file)
     (flush)
     (action (dynamic-require (build-path here file) provided))
     (flush)))
 test-files)

(printf "\nWARNING: didn't run color-test.rkt\n")
(flush)
