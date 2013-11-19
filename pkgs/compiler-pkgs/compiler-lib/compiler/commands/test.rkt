#lang racket/base
(require racket/cmdline
         racket/match
         racket/format
         racket/list
         racket/function
         racket/port
         racket/path
         raco/command-name
         rackunit/log
         pkg/lib)

(define submodules '())
(define run-anyways? #t)
(define quiet? #f)
(define quiet-program? #f)
(define table? #f)

(define (dynamic-require* p d)
  (parameterize
      ([current-output-port
        (if quiet-program?
          (open-output-nowhere)
          (current-output-port))]
       [current-error-port
        (if quiet-program?
          (open-output-nowhere)
          (current-error-port))])
    (dynamic-require p d)))

(struct summary (failed total label body-res))
(define-syntax-rule (with-summary label . body)
  (let ()
    (match-define (cons before-failed before-total)
                  (test-log #:display? #f #:exit? #f))
    (define res (begin . body))
    (match-define (cons after-failed after-total)
                  (test-log #:display? #f #:exit? #f))
    (summary (- after-failed before-failed)
             (- after-total before-total)
             label
             res)))

(define (iprintf i fmt . more)
  (for ([j (in-range i)])
    (display #\space))
  (apply printf fmt more))
(define (display-summary top)
  (define files
    (let flatten ([sum top])
      (match sum
        [(list sum ...)
         (append-map flatten sum)]
        [(summary failed total `(file ,p) body)
         (list sum)]
        [(summary failed total label body)
         (flatten body)]
        [(? void?)
         empty])))
  (define sfiles
    (sort files
          (λ (x y)
            (cond
              [(= (summary-failed x) (summary-failed y))
               (> (summary-total x) (summary-total y))]
              [else
               (< (summary-failed x) (summary-failed y))]))))
  (define (max-width f)
    (string-length
     (number->string
      (apply max (map f sfiles)))))
  (define failed-wid (max-width summary-failed))
  (define total-wid (max-width summary-total))
  (for ([f (in-list sfiles)])
    (match-define (summary failed total `(file ,p) _) f)
    (displayln (~a (~a #:min-width failed-wid
                       #:align 'right
                       (if (zero? failed)
                         ""
                         failed))
                   " "
                   (~a #:min-width total-wid
                       #:align 'right
                       total)
                   " " p))))

(define (do-test e [check-suffix? #f])
  (match e
    [(? string? s)
     (do-test (string->path s))]
    [(? path? p)
     (cond
       [(directory-exists? p)
        (with-summary
         `(directory ,p)
         (map
          (λ (dp)
            (do-test (build-path p dp) #t))
          (directory-list p)))]
       [(and (file-exists? p)
             (or (not check-suffix?)
                 (regexp-match #rx#"\\.rkt$" (path->bytes p))))
        (with-summary
         `(file ,p)
         (parameterize ([current-command-line-arguments '#()])
           (define something-wasnt-declared? #f)
           (for ([submodule (in-list (if (null? submodules)
                                       '(test)
                                       (reverse submodules)))])
             (define mod `(submod ,p ,submodule))
             (cond
               [(module-declared? mod #t)
                (unless quiet?
                  (printf "raco test: ~s\n" `(submod ,(if (absolute-path? p)
                                                        `(file ,(path->string p))
                                                        (path->string p))
                                                     ,submodule)))
                (dynamic-require* mod 0)]
               [else
                (set! something-wasnt-declared? #t)]))
           (when (and run-anyways? something-wasnt-declared?)
             (unless quiet?
               (printf "raco test: ~s\n" (if (absolute-path? p)
                                           `(file ,(path->string p))
                                           (path->string p))))
             (dynamic-require* p 0))))]
       [(not (file-exists? p))
        (error 'test "given path ~e does not exist" p)])]))

(module paths racket/base
  (require setup/link
           racket/match
           racket/list)

  (struct col (name path) #:transparent)

  (define (get-linked file user? version?)
    (define version-re
      (and version?
           (regexp-quote (version))))
    (append
     (for/list ([c+p
                 (in-list
                  (links #:file file #:user? user? #:version-regexp version-re
                         #:with-path? #t))])
       (col (car c+p)
            (cdr c+p)))
     (for/list ([cp
                 (in-list
                  (links #:file file #:user? user? #:version-regexp version-re
                         #:root? #t))]
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
      (for*/list ([file (in-list (current-library-collection-links))]
                  [user? (in-list '(#t #f))]
                  [version? (in-list '(#t #f))])
        (get-linked file user? version?)))))

  ;; This should be in Racket somewhere and return all the collection
  ;; paths, rather than just the first as collection-path does.
  (define (collection-paths c)
    (match-define (list-rest sc more) (map path->string (explode-path c)))
    (append*
     (for/list ([col (all-collections)]
                #:when (string=? sc (col-name col)))
       (define p (col-path col))
       (define cp (apply build-path p more))
       (if (directory-exists? cp)
         (list cp)
         empty))))

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
        (with-summary
         `(collection ,e)
         (map do-test l))])]
    [packages?
     (define pd (pkg-directory e))
     (if pd
       (with-summary
        `(package ,e)
        (do-test pd))
       (error 'test "Package ~e is not installed" e))]
    [else
     (do-test e)]))

(command-line
 #:program (short-program+command-name)
 #:multi
 [("--submodule" "-s") name
  "Runs submodule <name>\n    (defaults to running just the `test' submodule)"
  (let ([n (string->symbol name)])
    (set! submodules (cons n submodules)))]
 #:once-any
 [("--run-if-absent" "-r")
  "Require module if submodule is absent (on by default)"
  (set! run-anyways? #t)]
 [("--no-run-if-absent" "-x")
  "Require nothing if submodule is absent"
  (set! run-anyways? #f)]
 #:once-each
 [("--quiet" "-q")
  "Suppress `raco test: ...' message"
  (set! quiet? #t)]
 [("--table" "-t")
  "Print a summary table"
  (set! table? #t)]
 [("--quiet-program" "-Q")
  "Quiet the program"
  (set! quiet-program? #t)]
 #:once-any
 [("--collection" "-c")
  "Interpret arguments as collections"
  (set! collections? #t)]
 [("--package" "-p")
  "Interpret arguments as packages"
  (set! packages? #t)]
 #:args file-or-directory
 (begin (define sum (map do-test-wrap file-or-directory))
        (when table?
          (display-summary sum))
        (void (test-log #:display? #t #:exit? #t))))
