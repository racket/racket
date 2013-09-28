#lang racket
(require racket/runtime-path compiler/compiler
         rackunit rackunit/text-ui
         typed-racket/optimizer/logging
         "../send-places.rkt")

(provide optimization-tests missed-optimization-tests
         test-opt test-missed-optimization test-file?
         generate-log tests-dir missed-optimizations-dir)

(define (get-expected-results file)
  (with-input-from-file file
    (lambda () ; from the test file
      (read-line) ; skip the #;#;
      (values (for/list ((l (in-lines (open-input-string (read))))) l)
              (read)))))

;; we log optimizations and compare to an expected log to make sure that all
;; the optimizations we expected did indeed happen
(define (compare-logs name dir promised-logs)
  (test-suite
    (format "Log Comparison for ~a" name)
    (test-begin
      (define-values (log output) (force promised-logs))
      (define-values (expected-log expected-output)
        (get-expected-results (build-path dir name)))

      (check-equal? log expected-log)
      (check-equal? output expected-output))))


(define-runtime-path tests-dir                "./tests")
(define-runtime-path missed-optimizations-dir "./missed-optimizations")

;; these two return lists of tests to be run for that category of tests
(define (test-opt name)
  (compare-logs name
                tests-dir
                (delay/thread (generate-log name tests-dir))))
(define (test-missed-optimization name)
  (compare-logs name
                missed-optimizations-dir
                (delay/thread (generate-log name missed-optimizations-dir))))

(define (test-file? name)
  (and (regexp-match ".*rkt$" name)
       ;; skip emacs temp unsaved file backups
       (not (regexp-match "^\\.#" name))))

(define (mk-test-opt name logs)
  (compare-logs name tests-dir logs))
(define (mk-test-missed-optimization name logs)
  (compare-logs name missed-optimizations-dir logs))

;; proc returns the list of tests to be run on each file
(define (mk-suite suite-name dir proc)
  (test-suite suite-name
    (let* ((logs (for/hash ([name (directory-list dir)]
                            #:when (test-file? name))
                   (values name (delay/thread (generate-log name dir))))))
      (make-test-suite ""
        (for/list (((name logs) logs))
          (proc name logs))))))

(define (optimization-tests)
  (mk-suite "Optimization Tests"
            tests-dir
            mk-test-opt))
(define (missed-optimization-tests)
  (mk-suite "Missed Optimization Tests"
            missed-optimizations-dir
            mk-test-missed-optimization))
