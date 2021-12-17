#lang racket/base
(require racket/cmdline
         raco/command-name
         compiler/cm
         compiler/compiler
         compiler/compilation-path
         dynext/file
         setup/parallel-build
         setup/path-to-relative
         racket/match)

(module test racket/base)

(define verbose (make-parameter #f))
(define very-verbose (make-parameter #f))
(define disable-inlining (make-parameter #f))

(define disable-deps (make-parameter #f))
(define disable-const (make-parameter #f))
(define prefixes (make-parameter null))
(define assume-primitives (make-parameter #t))
(define worker-count (make-parameter 1))

(define coll-paths (make-parameter null))

(define mzc-symbol (string->symbol (short-program+command-name)))

(define source-file-paths
  (command-line
   #:program (short-program+command-name)
   #:multi
   [("-l") path "Compile <path> interpreted as a collection-based module path"
    (coll-paths (cons path (coll-paths)))]
   #:once-each
   [("-j") n "Compile with up to <n> tasks in parallel" 
    (let ([num (string->number n)])
      (unless num (raise-user-error (format "~a: bad count for -j: ~s"
                                            (short-program+command-name) 
                                            n)))
      (worker-count num))]
   [("--disable-inline") "Disable procedure inlining during compilation"
    (disable-inlining #t)]
   [("--disable-constant") "Disable enforcement of module constants"
    (disable-const #t)]
   [("--no-deps") "Compile immediate files without updating dependencies"
    (disable-deps #t)]
   [("-p" "--prefix") file "Add elaboration-time prefix file for --no-deps"
    (prefixes (append (prefixes) (list file)))]
   [("--no-prim") "Do not assume `scheme' bindings at top level for --no-deps"
    (assume-primitives #f)]
   [("-v") "Verbose mode"
    (verbose #t)]
   [("--vv") "Very verbose mode"
    (verbose #t)
    (very-verbose #t)]
   #:args files
   (when (and (null? files)
              (null? (coll-paths)))
     (raise-user-error (format "~a: expects at least one file or module path"
                               (short-program+command-name))))
   files))

(define source-files
  (append source-file-paths
          (for/list ([lib-path (in-list (coll-paths))])
            (resolved-module-path-name 
             ((current-module-name-resolver) `(lib ,lib-path) #f #f #f)))))

(cond 
  ;; Just compile one file:
  [(disable-deps)
    (let ([prefix
           `(begin
              (require scheme)
              ,(if (assume-primitives)
                   '(void)
                   '(namespace-require/copy 'scheme))
              ,@(map (lambda (s) `(load ,s)) (prefixes))
              (void))])
      ((compile-zos prefix #:verbose? (verbose))
       source-files
       'auto))]
  ;; Normal make:
  [(= (worker-count) 1)
    (let ([n (make-base-empty-namespace)]
          [did-one? #f])
      (parameterize ([current-namespace n]
                     [manager-trace-handler
                      (if (very-verbose)
                          (λ (p) (printf "  ~a\n" p))
                          (manager-trace-handler))]
                     [manager-compile-notify-handler
                      (lambda (p)
                        (set! did-one? #t)
                        (when (verbose)
                          (printf "  making ~s\n" p)))])
        (for ([file source-files])
          (define file-name (if (string? file) file (path->string file)))
          (unless (file-exists? file)
            (error mzc-symbol "file does not exist: ~a" file-name))
          (set! did-one? #f)
          (let ([name (extract-base-filename/ss file mzc-symbol)])
            (when (verbose)
              (printf "\"~a\":\n" file-name))
            (parameterize ([compile-context-preservation-enabled
                            (disable-inlining)]
                           [compile-enforce-module-constants
                            (not (disable-const))])
              (managed-compile-zo file))
            (when (verbose)
              (printf " [~a \"~a\"]\n"
                      (if did-one? "output to" "already up-to-date at")
                      (get-compilation-bytecode-file file)))))))]
  ;; Parallel make:
  [else
   (define path-cache (make-hash))
   (or (parallel-compile-files
        source-files
        #:worker-count (worker-count)
        #:handler (lambda (id type work msg out err)
                    (define (->rel p)
                      (path->relative-string/library p #:cache path-cache))
                    (match type
                      ['start (when (verbose) (printf " ~a making ~a\n" id (->rel work)))]
                      ['done (when (verbose) (printf " ~a made ~a\n" id (->rel work)))]
                      ['output (printf " ~a output from: ~a\n~a~a" id work out err)]
                      [_ (printf " ~a error compiling ~a\n~a\n~a~a" id work msg out err)]))
        #:options (let ([cons-if-true (lambda (bool carv cdrv)
                                        (if bool
                                            (cons carv cdrv)
                                            cdrv))])
                    (cons-if-true
                     (disable-const)
                     'disable-constant
                     (cons-if-true
                      (very-verbose)
                      'very-verbose
                      (cons-if-true (disable-inlining) 'disable-inlining null)))))
       (exit 1))])
