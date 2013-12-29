#lang racket/base
(require racket/cmdline
         racket/match
         racket/format
         racket/list
         racket/function
         racket/port
         racket/path
         racket/place
         racket/future
         racket/file
         compiler/find-exe
         raco/command-name
         racket/system
         rackunit/log
         pkg/lib
         setup/collects
         setup/getinfo)

(define submodules '()) ; '() means "default"
(define first-avail? #f)
(define run-anyways? #t)
(define quiet? #f)
(define quiet-program? #f)
(define check-stderr? #f)
(define table? #f)

(define jobs 0) ; 0 mean "default"
(define task-sema (make-semaphore 1))

(define default-timeout #f) ; #f means "none"
(define default-mode #f) ; #f => depends on how many files are provided

(define single-file? #t)

;; Stub for running a test in a process:
(module process racket/base
  (require rackunit/log)
  ;; Arguments are a temp file to hold test results, the module
  ;; path to run, and the `dynamic-require` second argument:
  (define argv (current-command-line-arguments))
  (define result-file (vector-ref argv 0))
  (define test-module (read (open-input-string (vector-ref argv 1))))
  (define d (read (open-input-string (vector-ref argv 2))))

  (parameterize ([current-command-line-arguments '#()])
    (dynamic-require test-module d))

  (call-with-output-file*
   result-file
   #:exists 'truncate
   (lambda (o)
     (write (test-log #:display? #f #:exit? #f) o))))

;; Driver for running a test in a place:
(module place racket/base
  (require racket/place
           rackunit/log)
  (provide go)
  (define (go pch)
    (define l (place-channel-get pch))
    ;; Run the test:
    (parameterize ([current-command-line-arguments '#()]
                   [current-directory (caddr l)])
      (dynamic-require (car l) (cadr l)))
    ;; If the tests use `rackunit`, collect result stats:
    (define test-results 
      (test-log #:display? #f #:exit? #f))
    ;; Return test results. If we don't get this far, the result
    ;; code of the place determines whether it the test counts as
    ;; successful.
    (place-channel-put pch
                       ;; If the test did not use `rackunit`, claim
                       ;; success:
                       (if (zero? (car test-results))
                           (cons 0 1)
                           test-results))))

;; Run each test in its own place or process, and collect both test
;; results and whether any output went to stderr.
(define (dynamic-require-elsewhere p d
                                   #:mode [mode (or default-mode
                                                    (if single-file?
                                                        'direct
                                                        'process))]
                                   #:timeout [timeout default-timeout])
  (define c (make-custodian))
  (with-handlers ([exn:fail? (lambda (exn)
                               (custodian-shutdown-all c)
                               (unless quiet?
                                 (eprintf "~a: ~a\n" 
                                          (extract-file-name p)
                                          (exn-message exn)))
                               (summary 1 1 (current-label) #f))])
    (define e (open-output-bytes))

    (define stdout (if quiet-program?
                       (open-output-nowhere)
                       (current-output-port)))
    (define stderr (if quiet-program?
                       e
                       (if check-stderr?
                           (tee-output-port (current-error-port) e)
                           (current-error-port))))

    (define-values (result-code test-results)
      (case mode
        [(direct)
         (define pre (test-log #:display? #f #:exit? #f))
         (define done? #f)
         (define t
           (parameterize ([current-output-port stdout]
                          [current-error-port stderr]
                          [current-command-line-arguments '#()])
             (thread
              (lambda ()
                (dynamic-require p d)
                (set! done? #t)))))
         (unless (thread? (sync/timeout timeout t))
           (error 'test "timeout after ~a seconds" timeout))
         (unless done?
           (error 'test "test raised an exception"))
         (define post (test-log #:display? #f #:exit? #f))
         (values 0
                 (cons (- (car post) (car pre))
                       (- (cdr post) (cdr pre))))]
        [(place)
         ;; Start the test place:
         (define-values (pl in out/f err/f)
           (parameterize ([current-custodian c])
             (dynamic-place* '(submod compiler/commands/test place)
                             'go
                             #:in (current-input-port)
                             #:out stdout
                             #:err stderr)))
         
         ;; Send the module path to test:
         (place-channel-put pl (list p d (current-directory)))

         ;; Wait for the place to finish:
         (unless (sync/timeout timeout (place-dead-evt pl))
           (error 'test "timeout after ~a seconds" timeout))

         ;; Get result code and test results:
         (values (place-wait pl)
                 (sync/timeout 0 pl))]
        [(process)
         (define tmp-file (make-temporary-file))
         (define ps
           (parameterize ([current-output-port stdout]
                          [current-error-port stderr]
                          [current-subprocess-custodian-mode 'kill]
                          [current-custodian c])
             (process*/ports stdout
                             (current-input-port)
                             stderr
                             (find-exe)
                             "-l"
                             "racket/base"
                             "-e"
                             "(dynamic-require '(submod compiler/commands/test process) #f)"
                             tmp-file
                             (format "~s" (normalize-module-path p))
                             (format "~s" d))))
         (define proc (list-ref ps 4))
         
         (unless (sync/timeout timeout (thread (lambda () (proc 'wait))))
           (error 'test "timeout after ~a seconds" timeout))

         (define results
           (with-handlers ([exn:fail:read? (lambda () #f)])
             (call-with-input-file* tmp-file read)))
         
         (values (proc 'exit-code)
                 (and (pair? results)
                      (exact-positive-integer? (car results))
                      (exact-positive-integer? (cdr results))
                      results))]))
    
    ;; Shut down the place/process (usually a no-op unless it timed out):
    (custodian-shutdown-all c)

    ;; Check results:
    (when check-stderr?
      (unless (equal? #"" (get-output-bytes e))
        (error 'test "non-empty stderr: ~e" (get-output-bytes e))))
    (unless (zero? result-code)
      (error 'test "non-zero exit: ~e" result-code))
    (cond
     [test-results
      (summary (car test-results) (cdr test-results) (current-label) #f)]
     [else
      (summary 0 1 (current-label) #f)])))

;; For recording stderr while also propagating to the original stderr:
(define (tee-output-port p1 p2)
  (make-output-port
   (object-name p1)
   p1
   (lambda (bstr start end non-block? enable-break?)
     (cond
      [(= start end)
       (flush-output p1)
       0]
      [else
       (define n (write-bytes-avail* bstr p1 start end))
       (cond
        [(or (not n)
             (zero? n))
         (wrap-evt p1 (lambda (v) 0))]
        [else
         (write-bytes bstr p2 start (+ start n))
         n])]))
   (lambda () 
     (close-output-port p1)
     (close-output-port p2))))

(define (extract-file-name p)
  (cond
   [(and (pair? p) (eq? 'submod (car p)))
    (cadr p)]
   [else p]))

(define (add-submod mod sm)
  (if (and (pair? mod) (eq? 'submod (car mod)))
      (append mod '(config))
      (error 'test "cannot add test-config submodule to path: ~s" mod)))

(define (dynamic-require* p d try-config?)
  (define lookup
    (or (cond
         [(not try-config?) #f]
         [(module-declared? (add-submod p 'config) #t)
          (dynamic-require (add-submod p 'config) '#%info-lookup)]
         [else #f])
        (lambda (what get-default) (get-default))))
  (dynamic-require-elsewhere
   p d
   #:timeout (if default-timeout
                 (lookup 'timeout
                         (lambda () default-timeout))
                 +inf.0)))

(define current-label (make-parameter "???"))
(struct summary (failed total label body-res))

(define-syntax-rule (with-summary label . body)
  (call-with-summary label (lambda () . body)))

(define (call-with-summary label thunk)
  (define res
    ;; Produces either a summary or a list of summary:
    (parameterize ([current-label label])
      (thunk)))
  (if (summary? res)
      res
      (summary
       (apply + (map summary-failed res))
       (apply + (map summary-total res))
       (current-label)
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
      (apply max 0 (map f sfiles)))))
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

;; Like `map`, but allows `run-one-test`s in parallel while starting
;; tasks in the order that a plain `map` would run them. The #:sema
;; argument everywhere makes tests start in a deterministic order
;; and keeps a filesystem traversal from getting far ahead of the
;; test runs.
(define (map/parallel f l #:sema continue-sema)
  (cond
   [(jobs . <= . 1) (map (lambda (v) (f v #:sema continue-sema)) l)]
   [else
    (struct task (th result-box))
    (define ts
      (for/list ([i (in-list l)])
        (define b (box #f))
        (define c-sema (make-semaphore))
        (define t (thread
                   (lambda ()
                     (set-box! b (with-handlers ([exn? values])
                                   (f i #:sema c-sema)))
                     ;; If no parallel task was ever created,
                     ;; count that as progress to the parent
                     ;; thread:
                     (semaphore-post c-sema))))
        (sync c-sema)
        (task t b)))
    (semaphore-post continue-sema)
    (map sync (map task-th ts))
    (for/list ([t (in-list ts)])
      (define v (unbox (task-result-box t)))
      (if (exn? v)
          (raise v)
          v))]))

(define (normalize-module-path p)
  (cond
   [(path? p) (path->string p)]
   [(and (pair? p) (eq? 'submod (car p)))
    (list* 'submod (normalize-module-path (cadr p)) (cddr p))]
   [else p]))

(define ids '(1))
(define ids-lock (make-semaphore 1))

(define (set-jobs! n)
  (set! jobs n)
  (set! task-sema (make-semaphore jobs))
  (set! ids (for/list ([i (in-range jobs)]) i)))

;; Perform test of one module (in parallel, as allowed by
;; `task-sema`):
(define (test-module p mod try-config? #:sema continue-sema)
  (call-with-semaphore
   task-sema ; limits parallelism
   (lambda ()
     (semaphore-post continue-sema) ; allow next to try to start
     (define id
       (call-with-semaphore
        ids-lock
        (lambda ()
          (define id (car ids))
          (set! ids (cdr ids))
          (unless quiet?
            ;; in lock, so printouts are not interleaved
            (printf "raco test: ~a~s\n"
                    (if (jobs . <= . 1)
                        ""
                        (format "~a " id))
                    (let ([m (normalize-module-path p)])
                      (if (and (pair? mod) (eq? 'submod (car mod)))
                          (list* 'submod m (cddr mod))
                          m))))
          id)))
     (begin0
      (dynamic-require* mod 0 try-config?)
      (call-with-semaphore
       ids-lock
       (lambda ()
         (set! ids (cons id ids))))))))

;; Perform all tests in path `e`:
(define (test-files e
                    #:check-suffix? [check-suffix? #f]
                    #:sema continue-sema)
  (match e
    [(? string? s)
     (test-files (string->path s)
                 #:check-suffix? check-suffix?
                 #:sema continue-sema)]
    [(? path? p)
     (cond
       [(directory-exists? p)
        (set! single-file? #f)
        (if (omit-path? (path->directory-path p))
            (summary 0 0 #f 0)
            (with-summary
             `(directory ,p)
             (map/parallel
              (λ (dp #:sema s)
                (test-files (build-path p dp)
                            #:check-suffix? #t
                            #:sema s))
              (directory-list p)
              #:sema continue-sema)))]
       [(and (file-exists? p)
             (or (not check-suffix?)
                 (regexp-match #rx#"\\.rkt$" (path->bytes p)))
             (not (omit-path? p)))
        (parameterize ([current-directory (let-values ([(base name dir?) (split-path p)])
                                            (if (path? base)
                                                base
                                                (current-directory)))])
          (define file-name (file-name-from-path p))
          (with-summary
           `(file ,p)
           (let ([something-wasnt-declared? #f]
                 [did-one? #f])
             (filter
              values
              (append
               (for/list ([submodule (in-list (if (null? submodules)
                                                  '(test)
                                                  (reverse submodules)))])
                 (define mod `(submod ,file-name ,submodule))
                 (cond
                  [(and did-one? first-avail?)
                   #f]
                  [(module-declared? mod #t)
                   (set! did-one? #t)
                   (test-module p mod #t #:sema continue-sema)]
                  [else
                   (set! something-wasnt-declared? #t)
                   #f]))
               (list
                (and (and run-anyways? something-wasnt-declared?)
                     (test-module p file-name #f #:sema continue-sema))))))))]
       [(not (file-exists? p))
        (error 'test "given path ~e does not exist" p)]
       [else (summary 0 0 #f null)])]))

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

(define (test-top e #:sema continue-sema)
  (cond
    [collections?
     (match (collection-paths e)
       [(list)
        (error 'test "Collection ~e is not installed" e)]
       [l
        (with-summary
         `(collection ,e)
         (map/parallel test-files l #:sema continue-sema))])]
    [packages?
     (define pd (pkg-directory e))
     (if pd
       (with-summary
        `(package ,e)
        (test-files pd #:sema continue-sema))
       (error 'test "Package ~e is not installed" e))]
    [else
     (test-files e #:sema continue-sema)]))

;; --------------------------------------------------
;; Reading "info.rkt" files

(define omit-paths (make-hash))

(define collects-cache (make-hash))
(define info-done (make-hash))

(define (check-info p check-up?)
  (define-values (base name dir?) (split-path p))
  (define dir (normalize-info-path
               (if dir?
                   p
                   (if (path? base)
                       (path->complete-path base)
                       (current-directory)))))

  (when (and check-up? (not dir?))
    ;; Check enclosing collection
    (define c (path->collects-relative p #:cache collects-cache))
    (when (list? c)
      (check-info/parents dir
                          (apply build-path (map bytes->path (reverse (cdr (reverse (cdr c)))))))))

  (unless (hash-ref info-done dir #f)
    (hash-set! info-done dir #t)
    (define info (get-info/full dir))
    (when info
      (define v (info 'test-omit-paths (lambda () '())))
      (define (bad)
        (log-error "bad `test-omit-paths` in \"info.rkt\": ~e" v))
      (cond
       [(eq? v 'all)
        (hash-set! omit-paths dir #t)]
       [(list? v)
        (for ([i (in-list v)])
          (unless (path-string? i) (bad))
          (define p (normalize-info-path (path->complete-path i dir)))
          (define dp (if (directory-exists? p)
                         (path->directory-path p)
                         p))
          (hash-set! omit-paths dp #t))]
       [else (bad)]))))

(define (check-info/parents dir subpath)
  (let loop ([dir dir] [subpath subpath])
    (unless (hash-ref info-done dir #f)
      (check-info dir #f)
      (define-values (next-subpath subpath-name subpath-dir?) (split-path subpath))
      (define-values (next-dir dir-name dir-dir?) (split-path dir))
      (when (path? next-subpath)
        (loop next-dir next-subpath)))))

(define (normalize-info-path p)
  (simplify-path (path->complete-path p) #f))

(define (omit-path? p)
  (check-info p #t)
  (let ([p (normalize-info-path p)])
    (or (hash-ref omit-paths p #f)
        (let-values ([(base name dir?) (split-path p)])
          (and (path? base)
               (omit-path? base))))))

;; --------------------------------------------------

(define (string->number* what s check)
  (define n (string->number s))
  (unless (check n)
    (raise-user-error (string->symbol (short-program+command-name))
                      "invalid ~a: ~s"
                      what
                      s))
  n)

(command-line
 #:program (short-program+command-name)
 #:once-any
 [("--collection" "-c")
  "Interpret arguments as collections"
  (set! collections? #t)]
 [("--package" "-p")
  "Interpret arguments as packages"
  (set! packages? #t)]
 #:once-each
 [("--drdr")
  "Configure defaults to imitate DrDr"
  (set! first-avail? #t)
  (when (zero? jobs)
    (set-jobs! (processor-count)))
  (unless default-timeout
    (set! default-timeout 600))
  (set! check-stderr? #t)
  (set! quiet-program? #t)
  (set! table? #t)
  (unless default-mode
    (set! default-mode 'process))]
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
 [("--first-avail")
  "Run only the first available submodule"
  (set! first-avail? #f)]
 #:once-any
 [("--direct")
  "Run tests directly (default for a single file)"
  (set! default-mode 'direct)]
 [("--process")
  "Run tests in separate processes (default for multiple files)"
  (set! default-mode 'process)]
 [("--place")
  "Run tests in places"
  (set! default-mode 'place)]
 #:once-each
 [("--jobs" "-j") n
  "Run up to <n> tests in parallel"
  (set-jobs! (string->number* "jobs" n exact-positive-integer?))]
 [("--timeout") seconds
  "Set default timeout to <seconds>"
  (set! default-timeout (string->number* "timeout" seconds real?))]
 [("--quiet-program" "-Q")
  "Quiet the program"
  (set! quiet-program? #t)]
 [("--check-stderr" "-e")
  "Treat stderr output as a test failure"
  (set! check-stderr? #t)]
 [("--quiet" "-q")
  "Suppress `raco test: ...' message"
  (set! quiet? #t)]
 [("--table" "-t")
  "Print a summary table"
  (set! table? #t)]
 #:args file-or-directory
 (begin (unless (= 1 (length file-or-directory))
          (set! single-file? #f))
        (define sum
          ;; The #:sema argument everywhre makes tests start
          ;; in a deterministic order:
          (map/parallel test-top file-or-directory
                        #:sema (make-semaphore)))
        (when table?
          (display-summary sum))
        ;; Re-log failures and successes, and then report using `test-log`.
        ;; (This is awkward; is it better to not try to use `test-log`?)
        (for ([s (in-list sum)])
          (for ([i (in-range (summary-failed s))])
            (test-log! #f))
          (for ([i (in-range (- (summary-total s)
                                (summary-failed s)))])
            (test-log! #t)))
        (define r (test-log #:display? #t #:exit? #t))
        (exit (if (zero? (car r)) 0 1))))
