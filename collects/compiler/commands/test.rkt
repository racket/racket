#lang racket/base
(require racket/cmdline
         racket/match
         racket/path
         raco/command-name)

(define do-test
  (match-lambda
   [(? string? s)
    (do-test (string->path s))]
   [(? path? p)
    (cond
      [(directory-exists? p)
       (for-each
        (λ (dp)
          (do-test (build-path p dp)))
        (directory-list p))]
      [(file-exists? p)
       (define mod `(submod (file ,(path->string p)) test))
       (when (module-declared? mod #t)
         (dynamic-require mod #f))]
      [else
       (error 'test "Given path ~e does not exist" p)])]))

(command-line
 #:program (short-program+command-name)
 #:args files+directories
 (for-each do-test files+directories))
