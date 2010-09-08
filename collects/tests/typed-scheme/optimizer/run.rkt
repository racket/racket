#lang racket
(require racket/runtime-path racket/sandbox)

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
                                   (if optimize? "\n#:optimize\n" "\n")
                                   (substring prog (cdar m)))]
              [evaluator (make-module-evaluator prog)]
              [out       (get-output evaluator)])
         (kill-evaluator evaluator)
         out)))))

(define (generate-opt-log name)
  (parameterize ([current-load-relative-directory tests-dir]
                 [current-command-line-arguments  '#("--log-optimizations")])
    (let ((log-string
           (with-output-to-string
             (lambda ()
               (dynamic-require (build-path (current-load-relative-directory)
                                            name)
                                #f)))))
      ;; have the log as an sexp, since that's what the expected log is
      (with-input-from-string (string-append "(" log-string ")")
        read))))

(define (test gen)
  (let-values (((base name _) (split-path gen)))
    (when (show-names?) (displayln name))
    (or (not (regexp-match ".*rkt$" name)) ; we ignore all but racket files
        ;; we log optimizations and compare to an expected log to make sure
        ;; that all the optimizations we expected did indeed happen
        (and (or (let ((log      (generate-opt-log name))
                       ;; expected optimizer log, to see what was optimized
                       (expected
                        (with-input-from-file gen
                          (lambda ()
                            (read-line) ; skip the #;
                            (read)))))  ; get the log itself
                   (equal? log expected))
                 (begin (printf "~a failed: optimization log mismatch\n\n" name)
                        #f))
             ;; optimized and non-optimized versions must evaluate to the
             ;; same thing
             (or (equal? (evaluator gen) (evaluator gen #:optimize #t))
                 (begin (printf "~a failed: result mismatch\n\n" name)
                        #f))))))

(define to-run
  (command-line
   #:once-each
   ["--show-names" "show the names of tests as they are run" (show-names? #t)]
   ;; we optionally take a test name. if none is given, run everything (#f)
   #:args maybe-test-to-run
   (and (not (null? maybe-test-to-run))
        (car maybe-test-to-run))))

(define-runtime-path tests-dir "./tests")

(let ((n-failures
       (if to-run
           (if (test (format "tests/~a.rkt" to-run)) 0 1)
           (for/fold ((n-failures 0))
             ((gen (in-directory tests-dir)))
             (+ n-failures (if (test gen) 0 1))))))
  (unless (= n-failures 0)
    (error (format "~a tests failed." n-failures))))
