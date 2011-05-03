#lang racket
(require racket/runtime-path
         rackunit rackunit/text-ui)

(provide optimization-tests)

(define show-names? (make-parameter #f))

(define (generate-log name dir flags)
  ;; some tests require other tests, so some fiddling is required
  (parameterize ([current-load-relative-directory dir]
                 [current-command-line-arguments  flags])
    (let ((log-string
           (with-output-to-string
             (lambda ()
               (parameterize ([current-namespace (make-base-empty-namespace)])
                 (dynamic-require (build-path (current-load-relative-directory)
                                              name)
                                  #f))))))
      ;; have the log as an sexp, since that's what the expected log is
      (with-input-from-string (string-append "(" log-string ")")
        read))))

;; we log optimizations and compare to an expected log to make sure that all
;; the optimizations we expected did indeed happen
(define (compare-logs name dir flags)
  (test-suite "Log Comparison"
              (check-equal?
               ;; ugly, but otherwise rackunit spews the entire logs to
               ;; stderr, and they can be quite long
               #t
               (equal?
                ;; actual log
                (generate-log name dir flags)
                ;; expected log
                (with-input-from-file (build-path dir name)
                  (lambda () ; from the test file
                    (read-line) ; skip the #;
                    (read)))))))


(define-runtime-path tests-dir       "./tests")

;; these two return lists of tests to be run for that category of tests
(define (test-opt name)
  (list (compare-logs name tests-dir '#("--log-optimizations"))))

;; proc returns the list of tests to be run on each file
(define (mk-suite suite-name dir proc)
  (make-test-suite
   suite-name
   (for/list ([name (directory-list dir)]
              #:when (regexp-match ".*rkt$" name))
     (make-test-suite
      (path->string name)
      (cons (test-suite
             "Show Name"
             (check-eq? (begin (when (show-names?) (displayln name)) #t) #t))
            (proc name))))))

(define optimization-tests
  (mk-suite "Optimization Tests" tests-dir test-opt))


(define single-test
  (command-line
   #:once-each
   ["--show-names" "show the names of tests as they are run" (show-names? #t)]
   ;; we optionally take a test name. if none is given, run everything (#f)
   #:args maybe-test-to-run
   (and (not (null? maybe-test-to-run))
        (car maybe-test-to-run))))

(void ; to suppress output of the return value
 (run-tests
  (cond [single-test
         (let-values ([(base name _) (split-path single-test)])
           (make-test-suite "Single Test" (test-opt name)))]
        [else ; default = run everything
	 optimization-tests])
  'normal))
