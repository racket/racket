#lang racket/base
(require racket/cmdline
         racket/match
         racket/path
         raco/command-name)

(define submodule 'test)
(define run-anyways? #f)

(define (do-test e [check-suffix? #f])
  (match e
    [(? string? s)
     (do-test (string->path s))]
    [(? path? p)
     (cond
      [(directory-exists? p)
       (for-each
        (λ (dp)
           (do-test (build-path p dp) #t))
        (directory-list p))]
      [(and (file-exists? p)
            (or (not check-suffix?)
                (regexp-match #rx#"\\.rkt$" (path->bytes p))))
       (define mod `(submod ,p ,submodule))
       (cond
         [(module-declared? mod #t)
          (dynamic-require mod #f)]
         [(and run-anyways? (module-declared? p #t))
          (dynamic-require p #f)])]
      [(not (file-exists? p))
       (error 'test "Given path ~e does not exist" p)])]))

(command-line
 #:program (short-program+command-name)
 #:once-each
 [("--submodule" "-s") name
  "Runs submodule <name> (defaults to `test')"
  (set! submodule (string->symbol name))]
 [("--run-if-absent" "-r")
  "Require base module if submodule is absent"
  (set! run-anyways? #t)]
 #:args file-or-directory
 (for-each do-test file-or-directory))
