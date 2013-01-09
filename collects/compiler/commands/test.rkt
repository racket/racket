#lang racket/base
(require racket/cmdline
         racket/match
         racket/path
         raco/command-name)

(define submodule 'test)
(define run-anyways? #t)
(define collections? #f)

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
        (printf "testing ~a\n" p)
        (define mod `(submod ,p ,submodule))
        (cond
          [(module-declared? mod #t)
           (dynamic-require mod #f)]
          [(and run-anyways? (module-declared? p #t))
           (dynamic-require p #f)])]
       [(not (file-exists? p))
        (error 'test "Given path ~e does not exist" p)])]))

;; XXX This should be in Racket somewhere and return all the paths,
;; including the ones from the user and system collection links files (the system one is not specified in the docs, so I can't actually implement it correctly)
(define (all-library-collection-paths)
  (find-library-collection-paths))

;; XXX This should be in Racket somewhere and return all the
;; collection paths, rather than just the first as collection-path
;; does.
;;
;; This implementation is wrong, btw, because it would ignore
;; collect-only links
(define (collection-paths c)
  (for/list ([r (all-library-collection-paths)]
             #:when (directory-exists? (build-path r c)))
    (build-path r c)))

(define (do-test-wrap e)
  (cond
    [collections?
     (for-each do-test (collection-paths e))]
    [else
     (do-test e)]))

(command-line
 #:program (short-program+command-name)
 #:once-each
 [("--submodule" "-s") name
  "Runs submodule <name> (defaults to `test')"
  (set! submodule (string->symbol name))]
 [("--run-if-absent" "-r")
  "Require module if submodule is absent (on by default)"
  (set! run-anyways? #t)]
 [("--no-run-if-absent" "-x")
  "Require nothing if submodule is absent"
  (set! run-anyways? #f)]
 [("--collection" "-c")
  "Interpret arguments as collections"
  (set! collections? #t)]
 #:args file-or-directory
 (for-each do-test-wrap file-or-directory))
