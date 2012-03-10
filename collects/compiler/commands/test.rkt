#lang racket/base
(require racket/cmdline
         racket/match
         racket/path
         raco/command-name)

(define submodule 'test)
(define run-anyways? #f)

(define do-test
  (match-lambda
   [(? string? s)
    (do-test (string->path s))]
   [(? path? p)
    (define ps (path->string p))
    (cond
      [(directory-exists? p)
       (for-each
        (λ (dp)
          (do-test (build-path p dp)))
        (directory-list p))]
      [(and (file-exists? p)
            (regexp-match #rx"\\.rkt$" ps))
       (define fmod `(file ,ps))
       (define mod `(submod ,fmod ,submodule))
       (cond
         [(module-declared? mod #t)
          (dynamic-require mod #f)]
         [(and run-anyways? (module-declared? fmod #t))
          (dynamic-require fmod #f)])]
      [(not (file-exists? p))
       (error 'test "Given path ~e does not exist" p)])]))

(command-line
 #:program (short-program+command-name)
 #:once-each
 [("--submodule" "-s") submodule-str
  "Determines which submodule to load"
  (set! submodule (string->symbol submodule-str))]
 [("--run-if-absent" "-r")
  "When set, raco test will require the default module if the given submodule is not present."
  (set! run-anyways? #t)]
 #:args files+directories
 (for-each do-test files+directories))
