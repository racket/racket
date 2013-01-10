#lang racket/base
(require racket/cmdline
         racket/match
         racket/path
         raco/command-name
         planet2/lib)

(define submodule 'test)
(define run-anyways? #t)

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

(module paths racket/base
  (require setup/link
           racket/list)

  (struct col (name path) #:transparent)

  (define (get-linked user? version?)
    (define version-re
      (and version?
           (regexp-quote (version))))
    (append
     (for/list ([c+p (in-list (links #:user? user? #:version-regexp version-re #:with-path? #t))])
       (col (car c+p)
            (cdr c+p)))
     (for/list ([cp (in-list (links #:root? #t #:user? user? #:version-regexp version-re))]
                #:when (directory-exists? cp)
                [collection (directory-list cp)]
                #:when (directory-exists? (build-path cp collection)))
       (col (path->string collection)
            (build-path cp collection)))))

  ;; A list of `col's, where each collection may be represented
  ;; by multiple elements of the list, each with its own path.
  (define (all-collections)
    (remove-duplicates
     (append*
      (for/list ([cp (current-library-collection-paths)]
                 #:when (directory-exists? cp)
                 [collection (directory-list cp)]
                 #:when (directory-exists? (build-path cp collection)))
        (col (path->string collection)
             (build-path cp collection)))
      (for*/list ([user? (in-list '(#t #f))]
                  [version? (in-list '(#t #f))])
        (get-linked user? version?)))))

  ;; This should be in Racket somewhere and return all the collection
  ;; paths, rather than just the first as collection-path does.
  (define (collection-paths c)
    (for/list ([col (all-collections)]
               #:when (string=? c (col-name col)))
      (col-path col)))

  (provide collection-paths))

(require (submod "." paths))

(define collections? #f)
(define packages? #f)

(define (do-test-wrap e)
  (cond
    [collections?
     (match (collection-paths e)
       [(list)
        (error 'test "Collection ~e is not installed" e)]
       [l
        (for-each do-test l)])]
    [packages?
     (unless
         (for*/or ([civs (in-list '(#t #f))]
                   [cisw (in-list '(#f #t))])
           (define pd
             (parameterize ([current-install-version-specific? civs]
                            [current-install-system-wide? cisw])
               (with-handlers ([exn:fail? (λ (x) #f)])
                 (package-directory e))))
           (and pd (do-test pd)))
       (error 'test "Package ~e is not installed" e))]
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
 #:once-any
 [("--collection" "-c")
  "Interpret arguments as collections"
  (set! collections? #t)]
 [("--package" "-p")
  "Interpret arguments as packages"
  (set! packages? #t)]
 #:args file-or-directory
 (for-each do-test-wrap file-or-directory))
