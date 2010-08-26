#lang racket
(require racket/runtime-path racket/sandbox)

;; since Typed Scheme's optimizer does source to source transformations,
;; we compare the expansion of automatically optimized and hand optimized
;; modules
(define (read-and-expand file)
  ;; drop the type tables added by typed scheme, since they can be in a
  ;; different order each time, and that would make tests fail when they
  ;; shouldn't
  (filter
   ;; drop the "module", its name and its language, so that we can write
   ;; the 2 versions of each test in different languages (typed and
   ;; untyped) if need be
   (match-lambda [(list 'define-values-for-syntax '() _ ...) #f] [_ #t])
   (cadddr
    (syntax->datum
     (parameterize ([current-namespace (make-base-namespace)]
                    [read-accept-reader #t])
       (with-handlers
           ([exn:fail? (lambda (exn)
                         (printf "~a\n" (exn-message exn))
                         #'(#f #f #f (#f)))]) ; for cadddr
         (expand (with-input-from-file file read-syntax))))))))


;; the first line must be the #lang line
;; the second line must be #:optimize
(define (evaluator file #:optimize [optimize? #f])
  (call-with-trusted-sandbox-configuration
   (lambda ()
     (parameterize ([current-load-relative-directory
                     (build-path here "generic")]
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

(define (test gen)
  (let-values (((base name _) (split-path gen)))
    (or (regexp-match ".*~" name) ; we ignore backup files
        (directory-exists? gen)   ; and directories
        ;; machine optimized and hand optimized versions must expand to the
        ;; same code
        (and (or (equal? (parameterize ([current-load-relative-directory
                                         (build-path here "generic")])
                           (read-and-expand gen))
                         (let ((hand-opt-dir (build-path here "hand-optimized")))
                           (parameterize ([current-load-relative-directory hand-opt-dir])
                             (read-and-expand (build-path hand-opt-dir name)))))
                 (begin (printf "~a failed: expanded code mismatch\n\n" name)
                        #f))
             ;; optimized and non-optimized versions must evaluate to the
             ;; same thing
             (or (equal? (evaluator gen) (evaluator gen #:optimize #t))
                 (begin (printf "~a failed: result mismatch\n\n" name)
                        #f))))))

(define-runtime-path here ".")

(let ((n-failures
       (if (> (vector-length (current-command-line-arguments)) 0)
           (if (test (format "generic/~a.rkt"
                             (vector-ref (current-command-line-arguments) 0)))
               0 1)
           (for/fold ((n-failures 0))
             ((gen (in-directory (build-path here "generic"))))
             (+ n-failures (if (test gen) 0 1))))))
  (unless (= n-failures 0)
    (error (format "~a tests failed." n-failures))))
