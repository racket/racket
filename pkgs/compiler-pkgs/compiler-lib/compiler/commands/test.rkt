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
         pkg/path
         setup/collects
         setup/getinfo)

(define rx:default-suffixes #rx#"\\.(?:rkt|scrbl)$")
;; For any other file suffix, a `test-command-line-arguments`
;; entry is required in "info.rkt".

(define submodules '()) ; '() means "default"
(define first-avail? #f)
(define run-anyways? #t)
(define quiet? #f)
(define quiet-program? #f)
(define check-stderr? #f)
(define table? #f)
(define fresh-user? #f)
(define empty-input? #f)
(define heartbeat-secs #f)
(define ignore-stderr-patterns null)

(define jobs 0) ; 0 mean "default"
(define task-sema (make-semaphore 1))

(define default-timeout #f) ; #f means "none"
(define default-mode #f) ; #f => depends on how many files are provided

(define single-file? #t)

(define lock-file-dir (or (getenv "PLTLOCKDIR")
                          (find-system-path 'temp-dir)))
(define max-lock-delay (or (let ([n (string->number (or (getenv "PLTLOCKTIME") ""))])
                             (and (real? n)
                                  n))
                           (* 4 60 60))) ; default: wait at most 4 hours

(define test-exe-name (string->symbol (short-program+command-name)))

;; Stub for running a test in a process:
(module process racket/base
  (require rackunit/log
           racket/file)
  ;; Arguments are a temp file to hold test results, the module
  ;; path to run, and the `dynamic-require` second argument:
  (define argv (current-command-line-arguments))
  (define result-file (vector-ref argv 0))
  (define test-module (read (open-input-string (vector-ref argv 1))))
  (define d (read (open-input-string (vector-ref argv 2))))
  (define args (list-tail (vector->list argv) 3))

  ;; In case PLTUSERHOME is set, make sure relevant
  ;; directories exist:
  (define (ready-dir d)
    (make-directory* d))
  (ready-dir (find-system-path 'doc-dir))

  (parameterize ([current-command-line-arguments (list->vector args)])
    (dynamic-require test-module d)
    ((executable-yield-handler) 0))

  (call-with-output-file*
   result-file
   #:exists 'truncate
   (lambda (o)
     (write (test-log #:display? #f #:exit? #f) o)))
  (exit 0))

;; Driver for running a test in a place:
(module place racket/base
  (require racket/place
           rackunit/log)
  (provide go)
  (define (go pch)
    (define l (place-channel-get pch))
    ;; Run the test:
    (parameterize ([current-command-line-arguments (list->vector
                                                    (cadddr l))]
                   [current-directory (caddr l)])
      (dynamic-require (car l) (cadr l))
      ((executable-yield-handler) 0))
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
(define (dynamic-require-elsewhere p d args
                                   #:id id
                                   #:mode [mode (or default-mode
                                                    (if single-file?
                                                        'direct
                                                        'process))]
                                   #:timeout timeout
                                   #:responsible responsible
                                   #:lock-name lock-name
                                   #:random? random?)
  (define c (make-custodian))
  (define timeout? #f)
  (with-handlers ([exn:fail? (lambda (exn)
                               (custodian-shutdown-all c)
                               (unless quiet?
                                 (eprintf "~a: ~a\n" 
                                          (extract-file-name p)
                                          (exn-message exn)))
                               (summary 1 1 (current-label) #f (if timeout? 1 0)))])
    (define (go)
      (define e (open-output-bytes))

      (define stdout (if quiet-program?
                         (open-output-nowhere)
                         (current-output-port)))
      (define stderr (if quiet-program?
                         e
                         (if check-stderr?
                             (tee-output-port (current-error-port) e)
                             (current-error-port))))
      (define stdin (if empty-input?
                        (open-input-bytes #"")
                        (current-input-port)))

      (unless quiet?
        (when responsible
          (fprintf stdout "raco test:~a @(test-responsible '~s)\n"
                   id
                   responsible))
        (when random?
          (fprintf stdout "raco test:~a @(test-random #t)\n"
                   id))
        (when lock-name
          (fprintf stdout "raco test:~a @(lock-name ~s)\n"
                   id
                   lock-name)))
      
      (define-values (result-code test-results)
        (case mode
          [(direct)
           (define pre (test-log #:display? #f #:exit? #f))
           (define done? #f)
           (define t
             (parameterize ([current-output-port stdout]
                            [current-error-port stderr]
                            [current-input-port stdin]
                            [current-command-line-arguments (list->vector args)])
               (thread
                (lambda ()
                  (dynamic-require p d)
                  ((executable-yield-handler) 0)
                  (set! done? #t)))))
           (unless (thread? (sync/timeout timeout t))
             (set! timeout? #t)
             (error test-exe-name "timeout after ~a seconds" timeout))
           (unless done?
             (error test-exe-name "test raised an exception"))
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
                               #:in stdin
                               #:out stdout
                               #:err stderr)))
           
           ;; Send the module path to test:
           (place-channel-put pl (list p d (current-directory) args))

           ;; Wait for the place to finish:
           (unless (sync/timeout timeout (place-dead-evt pl))
             (set! timeout? #t)
             (error test-exe-name "timeout after ~a seconds" timeout))

           ;; Get result code and test results:
           (values (place-wait pl)
                   (sync/timeout 0 pl))]
          [(process)
           (define tmp-file (make-temporary-file))
           (define tmp-dir (and fresh-user?
                                (make-temporary-file "home~a" 'directory)))
           (define ps
             (parameterize ([current-output-port stdout]
                            [current-error-port stderr]
                            [current-subprocess-custodian-mode 'kill]
                            [current-custodian c]
                            [current-environment-variables (environment-variables-copy
                                                            (current-environment-variables))])
               (when fresh-user?
                 (environment-variables-set! (current-environment-variables)
                                             #"PLTUSERHOME"
                                             (path->bytes tmp-dir))
                 (environment-variables-set! (current-environment-variables)
                                             #"TMPDIR"
                                             (path->bytes tmp-dir))
                 (environment-variables-set! (current-environment-variables)
                                             #"PLTADDONDIR"
                                             (path->bytes (find-system-path 'addon-dir))))
               (apply process*/ports
                      stdout
                      stdin
                      stderr
                      (find-exe)
                      "-l"
                      "racket/base"
                      "-e"
                      "(dynamic-require '(submod compiler/commands/test process) #f)"
                      tmp-file
                      (format "~s" (normalize-module-path p))
                      (format "~s" d)
                      args)))
           (define proc (list-ref ps 4))
           
           (unless (sync/timeout timeout (thread (lambda () (proc 'wait))))
             (set! timeout? #t)
             (error test-exe-name "timeout after ~a seconds" timeout))

           (define results
             (with-handlers ([exn:fail:read? (lambda () #f)])
               (call-with-input-file* tmp-file read)))

           (delete-file tmp-file)
           (when tmp-dir
             (delete-directory/files tmp-dir))
           
           (values (proc 'exit-code)
                   (and (pair? results)
                        (exact-positive-integer? (car results))
                        (exact-positive-integer? (cdr results))
                        results))]))
      
      ;; Shut down the place/process (usually a no-op unless it timed out):
      (custodian-shutdown-all c)

      ;; Check results:
      (when check-stderr?
        (unless (let ([s (get-output-bytes e)])
                  (or (equal? #"" s)
                      (ormap (lambda (p) (regexp-match? p s))
                             ignore-stderr-patterns)))
          (error test-exe-name "non-empty stderr: ~e" (get-output-bytes e))))
      (unless (zero? result-code)
        (error test-exe-name "non-zero exit: ~e" result-code))
      (cond
       [test-results
        (summary (car test-results) (cdr test-results) (current-label) #f 0)]
       [else
        (summary 0 1 (current-label) #f 0)]))

    ;; Serialize the above with a lock, if any:
    (if lock-name
        (call-with-file-lock/timeout
         #:max-delay max-lock-delay 
         (build-path lock-file-dir lock-name)
         'exclusive
         go
         (lambda () (error test-exe-name "could not obtain lock: ~s" lock-name)))
        (go))))

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
      (error test-exe-name "cannot add test-config submodule to path: ~s" mod)))

(define (dynamic-require* p d
                          #:id id
                          #:try-config? try-config?
                          #:args args
                          #:timeout timeout
                          #:responsible responsible
                          #:lock-name lock-name
                          #:random? random?)
  (define lookup
    (or (cond
         [(not try-config?) #f]
         [(module-declared? (add-submod p 'config) #t)
          (dynamic-require (add-submod p 'config) '#%info-lookup)]
         [else #f])
        (lambda (what get-default) (get-default))))
  (dynamic-require-elsewhere
   p d args
   #:id id
   #:responsible (lookup 'responsible
                         (lambda () responsible))
   #:timeout (if default-timeout
                 (lookup 'timeout
                         (lambda () timeout))
                 +inf.0)
   #:lock-name (lookup 'lock-name
                       (lambda () lock-name))
   #:random? (lookup 'random?
                     (lambda () random?))))

(define current-label (make-parameter "???"))
(struct summary (failed total label body-res timeout))

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
       res
       (apply + (map summary-timeout res)))))


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
        [(summary failed total `(file ,p) body timeout)
         (list sum)]
        [(summary failed total label body timeout)
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
    (match-define (summary failed total `(file ,p) _ _) f)
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
(define (test-module p mod
                     #:sema continue-sema
                     #:try-config? try-config?
                     #:args [args '()]
                     #:timeout [timeout +inf.0]
                     #:responsible [responsible #f]
                     #:lock-name [lock-name #f]
                     #:random? [random? #f])
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
            (printf "raco test: ~a~s~a\n"
                    (if (jobs . <= . 1)
                        ""
                        (format "~a " id))
                    (let ([m (normalize-module-path p)])
                      (if (and (pair? mod) (eq? 'submod (car mod)))
                          (list* 'submod m (cddr mod))
                          m))
                    (apply string-append
                           (for/list ([a (in-list args)])
                             (format " ~s" (format "~a" a)))))
            (flush-output))
          id)))
     (define heartbeat-sema (make-semaphore))
     (define heartbeat-t
       (and heartbeat-secs
            (thread (lambda ()
                      (let loop ()
                        (unless (sync/timeout heartbeat-secs heartbeat-sema)
                          (call-with-semaphore
                           ids-lock
                           (lambda ()
                             (printf "raco test: ~a[still on ~s]\n"
                                     (if (jobs . <= . 1)
                                         ""
                                         (format "~a " id))
                                     (let ([m (normalize-module-path p)])
                                       (if (and (pair? mod) (eq? 'submod (car mod)))
                                           (list* 'submod m (cddr mod))
                                           m)))))
                          (loop)))))))
     (begin0
      (dynamic-require* mod 0
                        #:id (if (jobs . <= . 1)
                                 ""
                                 (format " ~a" id))
                        #:try-config? try-config?
                        #:args args
                        #:timeout timeout
                        #:responsible responsible
                        #:lock-name lock-name
                        #:random? random?)
      (when heartbeat-t
        (semaphore-post heartbeat-sema)
        (sync heartbeat-t))
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
        (define dir-p (path->directory-path p))
        (check-info dir-p)
        (if (omit-path? dir-p)
            (summary 0 0 #f null 0)
            (with-summary
             `(directory ,p)
             (map/parallel
              (λ (dp #:sema s)
                (test-files (build-path p dp)
                            #:check-suffix? #t
                            #:sema s))
              (directory-list p)
              #:sema continue-sema)))]
       [(and (or (not check-suffix?)
                 (regexp-match rx:default-suffixes p)
                 (get-cmdline p #f #:check-info? #t))
             (or (not check-suffix?)
                 (not (omit-path? p  #:check-info? #t))))
        (unless check-suffix?
          ;; make sure "info.rkt" information is loaded:
          (check-info p))
        (define norm-p (normalize-info-path p))
        (define args (get-cmdline norm-p))
        (define timeout (get-timeout norm-p))
        (define lock-name (get-lock-name norm-p))
        (define responsible (get-responsible norm-p))
        (define random? (get-random norm-p))
        (parameterize ([current-directory (let-values ([(base name dir?) (split-path p)])
                                            (if (path? base)
                                                base
                                                (current-directory)))])
          (define file-name (file-name-from-path p))
          (define (test-this-module mod try-config?)
            (test-module p mod
                         #:try-config? try-config?
                         #:sema continue-sema
                         #:args args
                         #:timeout timeout
                         #:responsible responsible
                         #:lock-name lock-name
                         #:random? random?))
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
                  [(with-handlers ([exn:fail?
                                    (lambda (exn)
                                      ;; If there's an error, then try running
                                      ;; this submodule to let the error show.
                                      ;; Log a warning, just in case.
                                      (log-warning "submodule load failed: ~s"
                                                   (exn-message exn))
                                      'error)])
                     (and (module-declared? mod #t)
                          'ok))
                   => (lambda (mode)
                        (set! did-one? #t)
                        (test-this-module mod (eq? mode 'ok)))]
                  [else
                   (set! something-wasnt-declared? #t)
                   #f]))
               (list
                (and (and run-anyways? something-wasnt-declared?)
                     (test-this-module file-name #f))))))))]
       [else (summary 0 0 #f null 0)])]))

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
(define libraries? #f)
(define check-top-suffix? #f)

(define (test-top e 
                  #:check-suffix? check-suffix?
                  #:sema continue-sema)
  (cond
    [collections?
     (match (collection-paths e)
       [(list)
        (error test-exe-name
               (string-append "collection not found\n"
                              "  collection name: ~a")
               e)]
       [l
        (with-summary
         `(collection ,e)
         (map/parallel test-files l #:sema continue-sema))])]
    [libraries?     
     (define (find x)
       (define rmp ((current-module-name-resolver) x #f #f #f))
       (define p (resolved-module-path-name rmp))
       (and (file-exists? p) p))
     (match (find (string->symbol e))
       [#f
        (error test-exe-name
               (string-append "module not found\n"
                              "  module path: ~a")
               e)]
       [l
        (with-summary
         `(library ,l)
         (test-files l #:sema continue-sema))])]
    [packages?
     (define pd (pkg-directory e))
     (if pd
       (with-summary
        `(package ,e)
        (test-files pd #:sema continue-sema))
       (error test-exe-name
              (string-append "no such installed package\n"
                             "  package name: ~a")
              e))]
    [else
     (unless (or (file-exists? e)
                 (directory-exists? e))
       (error test-exe-name
              (string-append "no such file or directory\n"
                             "  path: ~a")
              e))
     (test-files e
                 #:check-suffix? check-suffix? 
                 #:sema continue-sema)]))

;; --------------------------------------------------
;; Reading "info.rkt" files

(define omit-paths (make-hash))
(define command-line-arguments (make-hash))
(define timeouts (make-hash))
(define lock-names (make-hash))
(define responsibles (make-hash))
(define randoms (make-hash))

(define pkg-cache (make-hash))
(define collects-cache (make-hash))
(define info-done (make-hash))

(define (check-dir-info p)
  (define-values (base name dir?) (split-path p))
  (define dir (normalize-info-path
               (if dir?
                   p
                   (if (path? base)
                       (path->complete-path base)
                       (current-directory)))))

  (unless (hash-ref info-done dir #f)
    (hash-set! info-done dir #t)
    (define info (get-info/full dir))
    (when info
      (define (bad what v)
        (log-error "bad `~a' in \"info.rkt\": ~e" what v))
      
      (define (get-members table what all-ok?)
        (define v (info what (lambda () '())))
        (cond
         [(and all-ok? (eq? v 'all))
          (hash-set! table dir #t)]
         [(list? v)
          (for ([i (in-list v)])
            (unless (path-string? i) (bad what v))
            (define p (normalize-info-path (path->complete-path i dir)))
            (define dp (if (directory-exists? p)
                           (path->directory-path p)
                           p))
            (hash-set! table dp #t))]
         [else (bad what v)]))
      (get-members omit-paths 'test-omit-paths #t)
      (get-members randoms 'test-randoms #t)
      
      (define (get-keyed table what check? #:ok-all? [ok-all? #f])
        (define a (info what (lambda () '())))
        (if (list? a)
            (for ([arg (in-list a)])
              (unless (and (list? arg)
                           (= 2 (length arg))
                           (or (path-string? (car arg))
                               (and ok-all?
                                    (eq? (car arg) 'all)))
                           (check? (cadr arg)))
                (bad what a))
              (hash-set! table
                         (normalize-info-path (if (eq? (car arg) 'all)
                                                  dir
                                                  (path->complete-path (car arg) dir)))
                         (cadr arg)))
            (bad what a)))

      (get-keyed command-line-arguments
                 'test-command-line-arguments 
                 (lambda (v) (and (list? v)
                                  (andmap path-string? v))))
      (get-keyed timeouts
                 'test-timeouts
                 (lambda (v) (real? v)))
      (get-keyed lock-names
                 'test-lock-names
                 (lambda (v) (or (not v)
                                 (and (string? v)
                                      (path-string? v)))))
      (get-keyed responsibles
                 'test-responsibles
                 ok-responsible?
                 #:ok-all? #t)
      (get-keyed randoms
                 'test-random
                 (lambda (v) (string? v))))))

(define (check-info/parents dir subpath)
  (let loop ([dir dir] [subpath subpath])
    (check-dir-info dir)
    (define-values (next-subpath subpath-name subpath-dir?) (split-path subpath))
    (define-values (next-dir dir-name dir-dir?) (split-path dir))
    (when (path? next-subpath)
      (loop next-dir next-subpath))))

(define (check-info p)
  (check-dir-info p)
  ;; Check enclosing collection
  (define-values (base name dir?) (split-path p))
  (define c (if dir?
                #f
                (path->collects-relative p #:cache collects-cache)))
  (when (list? c)
    (check-info/parents (if (path? base)
                            (path->complete-path base)
                            (current-directory)) ; got 'relative
                        (apply build-path (map bytes->path (reverse (cdr (reverse (cdr c)))))))))

(define (normalize-info-path p)
  (simplify-path (path->complete-path p) #f))

(define (omit-path? p #:check-info? [check-info? #f])
  (when check-info? (check-info p))
  (let ([p (normalize-info-path p)])
    (or (hash-ref omit-paths p #f)
        (let-values ([(base name dir?) (split-path p)])
          (and (path? base)
               (omit-path? base))))))

(define (get-cmdline p [default null] #:check-info? [check-info? #f])
  (when check-info? (check-info p))
  (hash-ref command-line-arguments
            (if check-info? (normalize-info-path p) p)
            default))

(define (get-timeout p)
  ;; assumes `(check-info p)` has been called and `p` is normalized
  (hash-ref timeouts p +inf.0))

(define (get-lock-name p)
  ;; assumes `(check-info p)` has been called and `p` is normalized
  (hash-ref lock-names p #f))

(define (get-responsible p)
  ;; assumes `(check-info p)` has been called and `p` is normalized
  (or (let loop ([p p])
        (or (hash-ref responsibles p #f)
            (let-values ([(base name dir?) (split-path p)])
              (and (path? base)
                   (loop base)))))
      ;; Check package authors:
      (let-values ([(pkg subpath) (path->pkg+subpath p #:cache pkg-cache)])
        (and pkg
             (let ([pkg-dir (if (path? subpath)
                                (apply build-path
                                       (drop-right (explode-path p)
                                                   (length (explode-path subpath))))
                                pkg)])
               (define info (get-info/full pkg-dir))
               (and info
                    (let ([v (info 'pkg-authors (lambda () #f))])
                      (and (ok-responsible? v)
                           v))))))))

(define (get-random p)
  ;; assumes `(check-info p)` has been called and `p` is normalized
  (hash-ref randoms p #f))

(define (ok-responsible? v)
  (or (string? v)
      (symbol? v)
      (and (list? v)
           (andmap (lambda (v) (or (symbol? v) (string? v)))
                   v))))

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
 [("--lib" "-l")
  "Interpret arguments as libraries"
  (set! libraries? #t)]
 [("--package" "-p")
  "Interpret arguments as packages"
  (set! packages? #t)]
 [("--modules" "-m")
  ("Interpret arguments as modules"
   "  (ignore argument unless \".rkt\", \".scrbl\", or enabled by \"info.rkt\")")
  (set! check-top-suffix? #t)]
 #:once-each
 [("--drdr")
  "Configure defaults to imitate DrDr"
  (set! check-top-suffix? #t)
  (set! first-avail? #t)
  (set! empty-input? #t)
  (when (zero? jobs)
    (set-jobs! (processor-count)))
  (unless default-timeout
    (set! default-timeout 90))
  (set! check-stderr? #t)
  (set! quiet-program? #t)
  (set! fresh-user? #t)
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
 [("--fresh-user")
  "Fresh PLTUSERHOME, etc., for each test"
  (set! fresh-user? #t)]
 [("--empty-stdin")
  "Call program with an empty stdin"
  (set! empty-input? #t)]
 [("--quiet-program" "-Q")
  "Quiet the program"
  (set! quiet-program? #t)]
 [("--check-stderr" "-e")
  "Treat stderr output as a test failure"
  (set! check-stderr? #t)]
 #:multi
 [("++ignore-stderr") pattern
  "Ignore standard error output if it matches #px\"<pattern>\""
  (set! ignore-stderr-patterns
        (cons (pregexp pattern) ignore-stderr-patterns))]
 #:once-each
 [("--quiet" "-q")
  "Suppress `raco test: ...' message"
  (set! quiet? #t)]
 [("--heartbeat")
  "Periodically report that a test is still running"
  (set! heartbeat-secs 5)]
 [("--table" "-t")
  "Print a summary table"
  (set! table? #t)]
 #:args file-or-directory
 (begin (unless (= 1 (length file-or-directory))
          (set! single-file? #f))
        (define sum
          ;; The #:sema argument everywhre makes tests start
          ;; in a deterministic order:
          (map/parallel (lambda (f #:sema s)
                          (test-top f
                                    #:check-suffix? check-top-suffix?
                                    #:sema s))
                        file-or-directory
                        #:sema (make-semaphore)))
        (when table?
          (display-summary sum))
        (unless (or (eq? default-mode 'direct)
                    (and (not default-mode) single-file?))
          ;; Re-log failures and successes, and then report using `test-log`.
          ;; (This is awkward; is it better to not try to use `test-log`?)
          (for ([s (in-list sum)])
            (for ([i (in-range (summary-failed s))])
              (test-log! #f))
            (for ([i (in-range (- (summary-total s)
                                  (summary-failed s)))])
              (test-log! #t))))
        (test-log #:display? #t #:exit? #f)
        (define sum1 (call-with-summary #f (lambda () sum)))
        (exit (cond
               [(positive? (summary-timeout sum1)) 2]
               [(positive? (summary-failed sum1)) 1]
               [else 0]))))
