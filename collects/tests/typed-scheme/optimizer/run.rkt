#lang racket
(require racket/runtime-path
         rackunit rackunit/text-ui)

(provide optimization-tests missed-optimization-tests
         test-opt test-missed-optimization)

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


(define-runtime-path tests-dir                "./tests")
(define-runtime-path missed-optimizations-dir "./missed-optimizations")

;; these two return lists of tests to be run for that category of tests
(define (test-opt name)
  (list (compare-logs name tests-dir '#("--log-optimizations"))))
(define (test-missed-optimization name)
  (list (compare-logs name missed-optimizations-dir '#("--log-missed-optimizations"))))

;; proc returns the list of tests to be run on each file
(define (mk-suite suite-name dir proc)
  (make-test-suite
   suite-name
   (for/list ([name (directory-list dir)]
              #:when (and (regexp-match ".*rkt$" name)
                          ;; skip emacs temp unsaved file backups
                          (not (regexp-match "^\\.#" name))))
     (make-test-suite
      (path->string name)
      (proc name)))))

(define optimization-tests
  (mk-suite "Optimization Tests" tests-dir test-opt))
(define missed-optimization-tests
  (mk-suite "Missed Optimization Tests" missed-optimizations-dir test-missed-optimization))
