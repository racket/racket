#lang racket/base

;; require this file to run all of the test suites for redex.

(require racket/runtime-path
         racket/cmdline
         racket/match
         pkg/lib
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
     "dq-test.rkt"
     "tl-test.rkt"
     "err-loc-test.rkt"
     "term-test.rkt"
     "rg-test.rkt"
     "gen-test.rkt"
     "keyword-macros-test.rkt"
     "core-layout-test.rkt"
     "pict-test.rkt"
     "hole-test.rkt"
     "stepper-test.rkt"
     "check-syntax-test.rkt"
     "test-docs-complete.rkt"
     "tut-subst-test.rkt"
     "enumerator-test.rkt"
     "enum-test.rkt"
     "bitmap-test.rkt")
   (if test-examples?
       '("<redex-examples>/redex/examples/cbn-letrec.rkt"
         "<redex-examples>/redex/examples/stlc.rkt"
         "<redex-examples>/redex/examples/pi-calculus.rkt"
         "<redex-examples>/redex/examples/list-machine/test.rkt"
         ("<redex-examples>/redex/examples/beginner.rkt" main)
         "<redex-examples>/redex/examples/racket-machine/reduction-test.rkt"
         "<redex-examples>/redex/examples/racket-machine/verification-test.rkt"
         "<redex-examples>/redex/examples/delim-cont/test.rkt"
         "<redex-examples>/redex/examples/cont-mark-transform/all-test.rkt"
         ("<redex-examples>/redex/examples/r6rs/r6rs-tests.rkt" main))
       '())))

(define-runtime-path here ".")
(define examples-path (pkg-directory "redex-examples"))

(define (flush)
  ;; these flushes are here for running under cygwin, 
  ;; which somehow makes racket think it isn't using
  ;; an interative port
  (flush-output (current-error-port))
  (flush-output (current-output-port)))

(for ([test-file (in-list test-files)])
  (define-values (file provided action)
    (match test-file
      [(list (? string? file) id)
       (values file id (λ (x) (x)))]
      [(? string?) 
       (values test-file #f values)]))
  (flush)
  (printf "running ~a\n" file)
  (flush)
  (define path
    (if (regexp-match #rx"<redex-examples>" file)
        (build-path examples-path (cadr (regexp-match #rx"^<redex-examples>/(.*)$" file)))
        (build-path here file)))
  (action (dynamic-require path provided))
  (flush))

(printf "\nWARNING: didn't run color-test.rkt\n")
(flush)


;; Test mode:
(module test racket/base
  (require syntax/location)
  (parameterize ([current-command-line-arguments
                  (vector "--examples" "--no-bitmap-gui")])
    (dynamic-require (quote-module-path "..") #f)))
