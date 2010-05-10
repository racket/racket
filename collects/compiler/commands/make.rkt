#lang scheme/base
(require scheme/cmdline
         raco/command-name
         compiler/cm
         "../compiler.ss"
         dynext/file)

(define verbose (make-parameter #f))
(define very-verbose (make-parameter #f))
(define disable-inlining (make-parameter #f))

(define disable-deps (make-parameter #f))
(define prefixes (make-parameter null))
(define assume-primitives (make-parameter #t))

(define source-files
  (command-line
   #:program (short-program+command-name)
   #:once-each
   [("--disable-inline") "Disable procedure inlining during compilation"
    (disable-inlining #t)]
   [("--no-deps") "Compile immediate files without updating depdencies"
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

(if (disable-deps)
    ;; Just compile one file:
    (let ([prefix
           `(begin
              (require scheme)
              ,(if (assume-primitives)
                   '(void)
                   '(namespace-require/copy 'scheme))
              (require compiler/cffi)
              ,@(map (lambda (s) `(load ,s)) (prefixes))
              (void))])
      ((compile-zos prefix #:verbose? (verbose))
       source-files
       'auto))
    ;; Normal make:
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
                          (printf "  making ~s\n" (path->string p))))])
        (for ([file source-files])
          (unless (file-exists? file)
            (error 'mzc "file does not exist: ~a" file))
          (set! did-one? #f)
          (let ([name (extract-base-filename/ss file 'mzc)])
            (when (verbose)
              (printf "\"~a\":\n" file))
            (parameterize ([compile-context-preservation-enabled
                            (disable-inlining)])
              (managed-compile-zo file))
            (let ([dest (append-zo-suffix
                         (let-values ([(base name dir?) (split-path file)])
                           (build-path (if (symbol? base) 'same base)
                                       "compiled" name)))])
              (when (verbose)
                (printf " [~a \"~a\"]\n"
                        (if did-one? "output to" "already up-to-date at")
                        dest))))))))
