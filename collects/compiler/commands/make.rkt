#lang scheme/base
(require scheme/cmdline
         raco/command-name
         compiler/cm
         "../compiler.rkt"
         dynext/file
         setup/parallel-build
         racket/match)

(define verbose (make-parameter #f))
(define very-verbose (make-parameter #f))
(define disable-inlining (make-parameter #f))

(define disable-deps (make-parameter #f))
(define disable-const (make-parameter #f))
(define prefixes (make-parameter null))
(define assume-primitives (make-parameter #t))
(define worker-count (make-parameter 1))

(define mzc-symbol (string->symbol (short-program+command-name)))

(define source-files
  (command-line
   #:program (short-program+command-name)
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
   #:args (file . another-file) (cons file another-file)))

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
                      (lambda (p)
                        (when (very-verbose)
                          (printf "  ~a\n" p)))]
                     [manager-compile-notify-handler
                      (lambda (p)
                        (set! did-one? #t)
                        (when (verbose)
                          (printf "  making ~s\n" p)))])
        (for ([file source-files])
          (unless (file-exists? file)
            (error mzc-symbol "file does not exist: ~a" file))
          (set! did-one? #f)
          (let ([name (extract-base-filename/ss file mzc-symbol)])
            (when (verbose)
              (printf "\"~a\":\n" file))
            (parameterize ([compile-context-preservation-enabled
                            (disable-inlining)]
                           [compile-enforce-module-constants
                            (not (disable-const))])
              (managed-compile-zo file))
            (let ([dest (append-zo-suffix
                         (let-values ([(base name dir?) (split-path file)])
                           (build-path (if (symbol? base) 'same base)
                                       "compiled" name)))])
              (when (verbose)
                (printf " [~a \"~a\"]\n"
                        (if did-one? "output to" "already up-to-date at")
                        dest)))))))]
  ;; Parallel make:
  [else 
   (parallel-compile-files 
    source-files
    #:worker-count (worker-count)
    #:handler (lambda (type work msg out err)
                (match type
                  ['done (when (verbose) (printf " Made ~a\n" work))]
                  ['output (printf " Output from: ~a\n~a~a" work out err)]
                  [else (printf " Error compiling ~a\n~a\n~a~a" work msg out err)]))
    #:options (let ([cons-if-true (lambda (bool carv cdrv)
                                    (if bool
                                        (cons carv cdrv)
                                        cdrv))])
                (cons-if-true 
                 (very-verbose) 
                 'very-verbose
                 (cons-if-true (disable-inlining) 'disable-inlining null))))])
