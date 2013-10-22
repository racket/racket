#lang racket

(require "run.rkt" "../send-places.rkt")

;; Assuming that only the log format changed, update test files to the
;; new format.
;; Running this after changes to the behavior of the optimizer is a
;; BAD idea, since in the case of regressions, the new broken behavior
;; would become the expected behavior.
;; Use this script with caution.

(define (transform file dir)
  (define (write-stringln s)
    (write-string s)
    (newline))
  ;; generate the new log, that will become the expected log
  (define-values (new-tr-log new-output) (generate-log file dir))
  (define source-code
    (call-with-input-file* (build-path dir file)
      (lambda (in)
        (read-line in) ; drop the #;#;
        (read in) ; drop the old expected tr log
        (read in) ; drop the old expected output
        (port->string in))))
  (with-output-to-file (build-path dir file) #:exists 'truncate
    (lambda ()
      (write-stringln "#;#;")
      (write-stringln "#<<END")
      (for ((entry new-tr-log))
        (write-stringln entry))
      (write-stringln "END")
      (if (regexp-match "\n" new-output)
          (begin
            (write-stringln "#<<END")
            (write-stringln new-output)
            (write-string "END"))
          (begin
            (write new-output)))
          (write-string source-code))))

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
      [else ; set of paths to transform (only works if run from the optimizer tests dir)
       (define results
         (for/list ([f (in-vector (current-command-line-arguments))])
           (define-values (dir-path file-name _) (split-path f))
           (define dir (path->string dir-path))
           (delay/thread
             (transform
               file-name
               (cond [(equal? dir "tests/")
                      tests-dir]
                     [(equal? dir "missed-optimizations/")
                      missed-optimizations-dir])))))
       (for-each force results)])
