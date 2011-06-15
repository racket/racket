#lang racket

(require "run.rkt")

;; Assuming that only the log format changed, update test files to the
;; new format.
;; Running this after changes to the behavior of the optimizer is a
;; BAD idea, since in the case of regressions, the new broken behavior
;; would become the expected behavior.
;; Use this script with caution.

(define (transform file dir)
  ;; generate the new log, that will become the expected log
  (define new-log (generate-log file dir))
  (define in (open-input-file (build-path dir file)))
  (read-line in) ; drop the #;
  (read in) ; drop the old expected log
  (let ([rest (port->string in)])
    (with-output-to-file (build-path dir file) #:exists 'truncate
      (lambda ()
        (displayln "#;")
        (displayln "(")
        (display new-log)
        (display ")")
        (display rest)))))

;; proc returns the list of tests to be run on each file
(define (transform-dir dir)
  (for/list ([name (directory-list dir)]
             #:when (test-file? name))
    (transform name dir)))

(transform-dir tests-dir)
(transform-dir missed-optimizations-dir)
