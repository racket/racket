#lang racket
(require racket/runtime-path racket/sandbox
         rackunit rackunit/text-ui)

(define show-names? (make-parameter #f))

(define prog-rx
  (pregexp (string-append "^\\s*"
                          "(#lang typed/(?:scheme|racket)(?:/base)?)"
                          "\\s+"
                          "#:optimize"
                          "\\s+")))

(define (evaluator file #:optimize [optimize? #f])
  (call-with-trusted-sandbox-configuration
   (lambda ()
     (parameterize ([current-load-relative-directory tests-dir]
                    [sandbox-memory-limit #f] ; TR needs memory
                    [sandbox-output 'string]
                    [sandbox-namespace-specs
                     (list (car (sandbox-namespace-specs))
                           'typed/racket
                           'typed/scheme)])
       ;; drop the expected log
       (let* ([prog (with-input-from-file file
                      (lambda ()
                        (read-line) ; drop #;
                        (read)      ; drop expected log
                        (port->string)))] ; get the actual program
              [m    (or (regexp-match-positions prog-rx prog)
                        (error 'evaluator "bad program contents in ~e" file))]
              [prog (string-append (substring prog (caadr m) (cdadr m))
                                   (if (not optimize?) "\n#:no-optimize\n" "\n")
                                   (substring prog (cdar m)))]
              [evaluator (make-module-evaluator prog)]
              [out       (get-output evaluator)])
         (kill-evaluator evaluator)
         out)))))

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
  (let ([path (build-path tests-dir name)])
    ;; we log optimizations and compare to an expected log to make sure that
    ;; all the optimizations we expected did indeed happen
    (list (compare-logs name tests-dir '#("--log-optimizations"))
          (test-suite
           ;; optimized and non-optimized versions must give the same result
           "Result Comparison"
           (check-equal? (evaluator path #:optimize #t)
                         (evaluator path))))))

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
