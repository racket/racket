#lang racket

(require "run.rkt" "../send-places.rkt")

;; Assuming that only the log format changed, update test files to the
;; new format.
;; Running this after changes to the behavior of the optimizer is a
;; BAD idea, since in the case of regressions, the new broken behavior
;; would become the expected behavior.
;; Use this script with caution.

(define (transform file dir)
  ;; generate the new log, that will become the expected log
  (define-values (new-tr-log new-output) (generate-log file dir))
  (define in (open-input-file (build-path dir file)))
  (read-line in) ; drop the #;#;
  (read in) ; drop the old expected tr log
  (read in) ; drop the old expected output
  (let ([rest (port->string in)])
    (with-output-to-file (build-path dir file) #:exists 'truncate
      (lambda ()
        (displayln "#;#;")
        (displayln "#<<END")
        (for ((entry new-tr-log))
          (displayln entry))
        (displayln "END")
        (if (regexp-match "\n" new-output)
            (begin
              (displayln "#<<END")
              (displayln new-output)
              (display "END"))
            (begin
              (write new-output)))
        (display rest)))))

;; proc returns the list of tests to be run on each file
(define (transform-dirs dirs)
  (define results
    (for*/list ([dir (in-list dirs)]
                [name (directory-list dir)]
                #:when (test-file? name))
      (delay/thread (transform name dir))))
  (for-each force results))

(start-workers)
(cond [(= (vector-length (current-command-line-arguments)) 0)
       (transform-dirs (list tests-dir missed-optimizations-dir))]
      [else ; set of paths to transform
       (define l (vector->list (current-command-line-arguments)))
       (for-each (lambda (f)
                   (define-values (path p b) (split-path f))
                   (define dir (path->string path))
                   ;; this only works if run from the optimizer tests dir
                   (transform
                    p
                    (cond [(equal? dir "tests/")
                           tests-dir]
                          [(equal? dir "missed-optimizations/")
                           missed-optimizations-dir])))
                 l)])
