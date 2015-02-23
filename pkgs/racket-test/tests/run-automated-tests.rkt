#lang racket/base

;; Is this obsolete? It has been disabled in DrDr for a while.
(module test racket/base)

;; To include a test, add an appropriate entry in `tests' below.
;; Notes:
;; - Each test is run in its own namespace, but there is very little
;;   additional sandboxing.  (There is a timeout of 15 minutes.)
;; - Specifically, the I/O ports are not diverted -- so please keep
;;   output to a minimum, preferably nothing if there are no errors.
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

;; Tests to run:
;; - Each should be a `test' call, with the path to the test file to
;;   require (relative to this script).
(define (all-tests)
  (test "racket/quiet.rktl" #:load? #t #:handler? #f
        ;; this *must* be in (lib ...) form, since that's the way that racket
        ;; uses the -I value to initialize the namespace, and there are tests
        ;; that expect to get (lib "racket/init") as a result.
        #:additional-modules '((lib "racket/init")))
  ;; (test "planet/lang.rkt")
  (test "typed-racket/nightly-run.rkt" #:timeout 25)
  (test "match/plt-match-tests.rkt")
  ;; (test "stepper/automatic-tests.rkt" #:additional-modules (scheme/base))
  (test "lazy/main.rkt")
  (test "scribble/main.rkt")
  (test "net/main.rkt")
  (test "file/main.rkt")
  (test "profile/main.rkt")
  (test "errortrace/alert.rkt")
  )

(require racket/runtime-path)

(define-runtime-path here ".")

(define exit-code 0)

(define (test path
              #:load? [load? #f] #:handler? [handler? #t]
              #:timeout [timeout 15] ; in minutes
              #:additional-modules [additional-modules '()])
  (define gloabl-state (current-preserved-thread-cell-values))
  (define stderr (current-error-port))
  (define (echo fmt . args)
    (flush-output (current-output-port))
    (flush-output (current-error-port))
    (fprintf stderr ">>> ~a: ~a\n" path (apply format fmt args)))
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
                (lambda ()
                  (sleep (* 60 timeout))
                  (echo "Timeout!")
                  (break-thread th)
                  (sleep 60)
                  (echo "BOOM! A minute has passed, killing the test thread!")
                  (kill-thread th)
                  (sleep 60)
                  (echo "Another minute passed, aborting!")
                  (abort 1 "Goodbye.")))))
    (parameterize* ([exit-handler
                     (lambda (n) (abort n "exit with error code ~a" n))]
                    [current-namespace (make-base-empty-namespace)])
      (for-each namespace-require additional-modules)
      (let ([thunk (lambda () ((if load? load namespace-require)
                               (build-path here path)))])
        (if handler?
          (with-handlers ([void (lambda (exn)
                                  (abort 1 "error: ~a" (exn-message exn)))])
            (thunk))
          (thunk)))
      (kill-thread timeout-thread)
      (echo "no failures.")))
  (current-preserved-thread-cell-values gloabl-state))

(all-tests)

(exit exit-code)
