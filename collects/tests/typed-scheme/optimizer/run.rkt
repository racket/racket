#lang racket
(require racket/runtime-path racket/sandbox)

;; the first line must be the #lang line
;; the second line must be #:optimize
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
       (let* ((lines (cdr (file->lines file))) ;; drop the #lang line
              (in    (if optimize?
                         lines
                         (cdr lines))) ;; drop the #:optimize
              (evaluator
               (make-evaluator 'typed/racket
                               (foldl (lambda (acc new)
                                        (string-append new "\n" acc))
                                      "" in)))
              (out (get-output evaluator)))
         (kill-evaluator evaluator)
         out)))))

(define (generate-opt-log name)
  (parameterize ([current-load-relative-directory tests-dir]
                 [current-command-line-arguments  '#("--log-optimizations")])
    (with-output-to-string
      (lambda ()
        (dynamic-require (build-path (current-load-relative-directory) name)
                         #f)))))

(define (test gen)
  (let-values (((base name _) (split-path gen)))
    (or (not (regexp-match ".*rkt$" name)) ; we ignore all but racket files
        ;; we log optimizations and compare to an expected log to make sure
        ;; that all the optimizations we expected did indeed happen
        (and (or (let ((log      (generate-opt-log name))
                       ;; expected optimizer log, to see what was optimized
                       (expected
                        (file->string
                         (build-path base
                                     (string-append (path->string name)
                                                    ".log")))))
                   (equal? log expected))
                 (begin (printf "~a failed: optimization log mismatch\n\n" name)
                        #f))
             ;; optimized and non-optimized versions must evaluate to the
             ;; same thing
             (or (equal? (evaluator gen) (evaluator gen #:optimize #t))
                 (begin (printf "~a failed: result mismatch\n\n" name)
                        #f))))))

(define-runtime-path tests-dir "./tests")

(let ((n-failures
       (if (> (vector-length (current-command-line-arguments)) 0)
           (if (test (format "tests/~a.rkt"
                             (vector-ref (current-command-line-arguments) 0)))
               0 1)
           (for/fold ((n-failures 0))
             ((gen (in-directory tests-dir)))
             (+ n-failures (if (test gen) 0 1))))))
  (unless (= n-failures 0)
    (error (format "~a tests failed." n-failures))))
