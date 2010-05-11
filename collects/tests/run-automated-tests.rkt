#lang racket/base

;; To include a test, add an appropriate entry in `tests' below.
;; Notes:
;; - Each test is run in its own namespace, but there is very little
;;   additional sandboxing.  (There is a timeout of 10 minutes.)
;; - Specifically, the I/O ports are not diverted -- so please keep
;;   output to a minimum, preferrably nothing if there are no errors.
;; - Tests are only running in racket (*not* gracket), but note that
;;   they will run with both the default 3m and the CGC executable, and
;;   with the JIT enabled and disabled.
;; - They will also run on all build platforms, some can be slow (eg,
;;   the Solaris build, or if we get an ARM build).  Many of the build
;;   machines are used by people, be polite!
;; - The tests are usually run from some temporary directory, you can
;;   create files in it, but DO NOT use any other paths: don't write
;;   into the PLT tree, don't write into the home directory (or rely
;;   on things in it), assume a fresh planet cache.
;; - To signal failures, either raise an error, or `exit' with a
;;   positive code.  Obviously, make sure that failures print some
;;   indicative text for you.
;; - A semi-related note: the PLT tree compilation should not rely on
;;   planet packages, so if you use them, make sure you add a
;;   `compile-omit-paths' in your test's info file.

;; Tests to run:
;;   Each should be a list with a mode symbol (`load' or `require'),
;;   the path to the test file (relative to this script) and module
;;   specifications for things to require into the initial namespace
;;   for the test before the test is loaded.  ('no-handler is a
;;   special flag that means that errors raised by the test suite are
;;   ignored, and should only be used by the racket tests.)
(define tests
  '([no-handler load "racket/quiet.rktl" (lib "racket/init")]
    ;; [require "planet/lang.rkt"]
    [require "typed-scheme/nightly-run.rkt"]
    [require "match/plt-match-tests.rkt"]
    ;; [require "stepper/automatic-tests.rkt" (lib "scheme/base")]
    [require "lazy/main.rkt"]
    [require "scribble/main.rkt"]
    [require "net/main.rkt"]
    [require "file/main.rkt"]
    [require "profile/main.rkt"]
    ))

(require racket/runtime-path)

(define-runtime-path here ".")

(define exit-code 0)

(for ([t tests])
  (define no-handler? (and (eq? 'no-handler (car t)) (set! t (cdr t))))
  (define name (cadr t))
  (define stderr (current-error-port))
  (define (echo fmt . args)
    (flush-output (current-output-port))
    (flush-output (current-error-port))
    (fprintf stderr ">>> ~a: ~a\n" name (apply format fmt args)))
  (newline stderr)
  (echo "running...")
  (let/ec break
    (define (abort n fmt . xs)
      (when (positive? n)
        (apply echo fmt xs)
        (set! exit-code (max exit-code n))
        (echo "BOOM!") ; used to find errors in nightly builds
        (break)))
    (define timeout-thread
      (thread (let ([th (current-thread)])
                (lambda () (sleep 1200) (echo "Timeout!") (break-thread th)))))
    (parameterize* ([exit-handler
                     (lambda (n) (abort n "exit with error code ~a" n))]
                    [current-namespace (make-base-empty-namespace)])
      (for-each namespace-require (cddr t))
      (let ([thunk (lambda ()
                     ((case (car t) [(load) load] [(require) namespace-require])
                      (build-path here name)))])
        (if no-handler?
          (thunk)
          (with-handlers ([void (lambda (exn)
                                  (abort 1 "error: ~a" (exn-message exn)))])
            (thunk))))
      (kill-thread timeout-thread)
      (echo "no failures found."))))

(exit exit-code)
