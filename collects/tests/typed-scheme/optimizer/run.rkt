#lang racket
(require racket/runtime-path
         rackunit rackunit/text-ui)

(provide optimization-tests missed-optimization-tests
         test-opt test-missed-optimization)

(define (with-logging-to-port port level proc)
  (let* ([logger   (current-logger)]
         [receiver (make-log-receiver logger level)]
         [stop-key (gensym)]
         [t (thread (lambda ()
                      (let loop ()
                        (let ([l (sync receiver)])
                          (unless (eq? (vector-ref l 2) stop-key)
                            (displayln (vector-ref l 1) ; actual message
                                       port)
                            (loop))))))])
    (begin0 (proc)
      (log-message logger level "" stop-key) ; stop the receiver thread
      (thread-wait t))))
;; TODO put in unstable somewhere


(define (generate-log name dir)
  ;; some tests require other tests, so some fiddling is required
  (let* ([log-port (open-output-string)]
         [out-string
          (with-output-to-string
            (lambda ()
              (with-logging-to-port log-port 'warning ; catch opt logs
               (lambda ()
                 (parameterize
                     ([current-namespace (make-base-empty-namespace)]
                      [current-load-relative-directory dir])
                   (dynamic-require
                    (build-path (current-load-relative-directory) name)
                    #f))))))])
    ;; have the log as an sexp, since that's what the expected log is
    (with-input-from-string
        (string-append "(" (get-output-string log-port) ; join log and results
                       " " out-string ")")
      read)))

;; we log optimizations and compare to an expected log to make sure that all
;; the optimizations we expected did indeed happen
(define (compare-logs name dir)
  (test-suite "Log Comparison"
              (check-equal?
               ;; ugly, but otherwise rackunit spews the entire logs to
               ;; stderr, and they can be quite long
               #t
               (equal?
                ;; actual log
                (generate-log name dir)
                ;; expected log
                (with-input-from-file (build-path dir name)
                  (lambda () ; from the test file
                    (read-line) ; skip the #;
                    (read)))))))


(define-runtime-path tests-dir                "./tests")
(define-runtime-path missed-optimizations-dir "./missed-optimizations")

;; these two return lists of tests to be run for that category of tests
(define (test-opt name)
  (list (compare-logs name tests-dir)))
(define (test-missed-optimization name)
  (list (compare-logs name missed-optimizations-dir)))

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
