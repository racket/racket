#lang racket/base
(require racket/cmdline
         racket/pretty
         racket/runtime-path
         (only-in racket/base
                  [eval host:eval]
                  [namespace-require host:namespace-require]
                  [current-library-collection-paths host:current-library-collection-paths]
                  [current-library-collection-links host:current-library-collection-links])
         compiler/depend
         "common/set.rkt"
         "main.rkt"
         "namespace/namespace.rkt"
         "common/module-path.rkt"
         "eval/module-read.rkt"
         "boot/kernel.rkt"
         "run/cache.rkt"
         "boot/runtime-primitive.rkt"
         "host/linklet.rkt"
         "run/status.rkt"
         "run/submodule.rkt"
         "host/correlate.rkt"
         "extract/main.rkt"
         (only-in "run/linklet.rkt" linklet-compile-to-s-expr))

(define-runtime-path main.rkt "main.rkt")

;; Record all files that contribute to the result
(define dependencies (make-hash))
(define extra-module-dependencies null)

(define extract? #f)
(define expand? #f)
(define linklets? #f)
(define checkout-directory #f)
(define cache-dir #f)
(define cache-read-only? #f)
(define cache-save-only #f)
(define cache-skip-first? #f)
(define time-expand? #f)
(define print-extracted-to #f)
(define check-dependencies #f)
(define dependencies-file #f)
(define makefile-dependencies-target #f)
(define makefile-dependencies-file #f)
(define extract-to-c? #f)
(define extract-to-decompiled? #f)
(define extract-to-bytecode? #f)
(define local-rename? #f)
(define no-global? #f)
(define global-ok (make-hasheq))
(define instance-knot-ties (make-hasheq))
(define primitive-table-directs (make-hasheq))
(define side-effect-free-modules (make-hash))
(define disallows null)
(define quiet-load? #f)
(define startup-module main.rkt)
(define submod-name #f)
(define load-file #f)
(define args
  (command-line
   #:once-any
   [("-x" "--extract") "Extract bootstrap linklet"
    (set! extract? #t)]
   [("-e" "--expand") "Expand instead of running"
    (set! expand? #t)]
   [("--linklets") "Compile to linklets instead of running"
    (set! linklets? #t)]
   [("-O") dir "Use and write bootstrap linklet to Racket checkout at <dir>"
    (set! checkout-directory (path->complete-path dir))
    (set! extract? #t)
    (set! extract-to-c? #t)
    (linklet-compile-to-s-expr #t)
    (set! print-extracted-to (build-path checkout-directory "src" "racket" "src" "startup.inc"))]
   #:once-each
   [("-k") dir "Use Racket checkout at <dir>"
    (set! checkout-directory (path->complete-path dir))]
   [("-c" "--cache") dir "Save and load from <dir>"
    (set! cache-dir (path->complete-path dir))]
   [("-r" "--read-only") "Use cache in read-only mode"
    (set! cache-read-only? #t)]
   [("-y" "--cache-only") file "Cache only for sources listed in <file>"
    (set! cache-save-only (call-with-input-file* file read))]
   [("-i" "--skip-initial") "Don't use cache for the initial load"
    (set! cache-skip-first? #t)]
   [("-s" "--s-expr") "Compile to S-expression instead of bytecode"
    (linklet-compile-to-s-expr #t)]
   [("-q" "--quiet") "Quiet load status"
    (set! quiet-load? #t)]
   [("--time") "Time re-expansion"
    (set! time-expand? #t)]
   [("-o" "--output") file "Print extracted bootstrap linklet to <file>"
    (when print-extracted-to (raise-user-error 'run "the `-O` flag implies `-o`, so don't use both"))
    (set! print-extracted-to file)]
   [("--check-depends") file "Skip if dependencies in <file> unchanged"
    (set! check-dependencies file)]
   [("--depends") file "Record dependencies in <file>"
    (set! dependencies-file file)]
   [("--makefile-depends") target file "Record makefile dependencies for <target> in <file>"
    (set! makefile-dependencies-target target)
    (set! makefile-dependencies-file file)]
   #:multi
   [("++depend") file "Record <file> as a dependency"
    (hash-set! dependencies (simplify-path (path->complete-path file)) #t)]
   [("++depend-module") mod-file "Add <mod-file> and transitive as dependencies"
                        (set! extra-module-dependencies (cons mod-file extra-module-dependencies))]
   #:once-any
   [("-C") "Print extracted bootstrap as a C encoding"
    (set! extract-to-c? #t)]
   [("-D") "Print extracted bootstrap as a decompiled"
    (set! extract-to-decompiled? #t)]
   [("-B") "Print extracted bootstrap as bytecode"
           (set! extract-to-bytecode? #t)]
   #:multi
   [("++disallow") id "If <id> is defined in the flattened version, explain why"
                   (set! disallows (cons (string->symbol id) disallows))]
   #:once-each
   [("--local-rename") "Use simpler names in extracted, instead of a unique name for each binding"
                       (set! local-rename? #t)]
   [("--no-global") "Complain if a variable looks like it holds mutable global state"
                    (set! no-global? #t)]
   #:multi
   [("++global-ok") id "Allow <id> as global state without complaint, after all"
                    (hash-set! global-ok (string->symbol id) #t)]
   #:multi
   [("++knot") primitive-table path ("Redirect imports from #%<primitive-table> to flattened from <path>;"
                                     " use `-` for <path> to leave as-is, effectively redirecting to a primitive use")
    (hash-update! instance-knot-ties
                  (string->symbol (format "#%~a" primitive-table))
                  (lambda (l) (cons (if (equal? path "-")
					'ignore
					(path->complete-path (normal-case-path path)))
				    l))
                  null)]
   [("++direct") primitive-table "Redirect from `(primitive-table '#%<primitive-table>)` to primitive use"
    (hash-set! primitive-table-directs
               (string->symbol (string-append "#%" primitive-table))
               "")]
   [("++direct-prefixed") primitive-table "Like ++direct, but prefix with <primitive-table>:"
    (hash-set! primitive-table-directs
               (string->symbol (string-append "#%" primitive-table))
               (string-append primitive-table ":"))]
   [("++pure") path "Insist that <path> is a module without side-effects"
    (hash-set! side-effect-free-modules (simplify-path (path->complete-path path)) #t)]
   #:once-any
   [("-t") file "Load specified file"
    (set! startup-module (path->complete-path file))]
   [("-l") lib "Load specified library"
    (set! startup-module `(lib ,lib))]
   [("-f") file "Load non-module file in `racket/base` namespace"
    (set! startup-module 'racket/base)
    (set! load-file file)]
   #:once-each
   [("--submod") name "Load specified submodule"
    (set! submod-name (string->symbol name))]
   #:args args args))

;; ----------------------------------------

;; If any `--check-depends` is specified, exit as soon as possible if
;; nothing's newer

(define (read-dependencies-from-file file)
  (and (file-exists? file)
       (with-handlers ([exn:fail:filesystem? (lambda (exn)
                                               (log-error (exn-message exn))
                                               #f)])
         (let ([l (call-with-input-file file read)])
           (and (list? l)
                (andmap bytes? l)
                (map bytes->path l))))))

(when check-dependencies
  (unless print-extracted-to
    (raise-user-error 'run "cannot check dependencies without a specific output file"))
  (define ts (file-or-directory-modify-seconds print-extracted-to #f (lambda () #f)))
  (when (and
         ts
         (let ([l (read-dependencies-from-file check-dependencies)])
           (and l
                (for/and ([dep (in-list l)])
                  (<= (file-or-directory-modify-seconds dep #f (lambda () +inf.0))
                      ts)))))
    (log-status "No dependencies are newer")
    (exit 0)))

;; ----------------------------------------

(define cache
  (and (or cache-dir extract?)
       (make-cache cache-dir (lambda (path)
                               (log-status "changed: ~a" path)))))

(when checkout-directory
  ;; After booting, we're going to change the way module paths
  ;; resolve. That's not generally ok, but as long we trigger visits
  ;; of available modules here, it turns out that it won't cause
  ;; trouble.
  (host:namespace-require ''#%kernel)
  (host:eval '(void)))

;; Install handlers:
(boot)

;; Avoid use of ".zo" files:
(use-compiled-file-paths null)

;; In case the host is in machine-independent mode, claim
;; machine-specific so the expander doesn't skip our extracting
;; linklet compiler:
(current-compile-target-machine (system-type 'target-machine))

;; Redirect module search to another installation:
(when checkout-directory
  (let ([l (list (build-path checkout-directory "collects"))])
    (host:current-library-collection-paths l))
  (let ([l (list #f
                 (build-path checkout-directory "share" "links.rktd"))])
    (host:current-library-collection-links l)))

(current-library-collection-paths (host:current-library-collection-paths))
(current-library-collection-links (host:current-library-collection-links))

;; Replace the load handler to stash compiled modules in the cache
;; and/or load them from the cache
(define orig-load (current-load))
(current-load (lambda (path expected-module)
                (cond
                 [expected-module
                  (let loop ()
                    (cond
                     [(and cache
                           (not cache-skip-first?)
                           (get-cached-compiled cache path
                                                (lambda ()
                                                  (when cache-dir
                                                    (unless quiet-load?
                                                      (log-status "cached: ~a" path))))))
                      => (lambda (m)
                           ;; Since we've set `use-compiled-file-paths` to null,
                           ;; the load/use-compiled handler thinks that we're
                           ;; always loading from source, so don't find the
                           ;; expected submodule with
                           ;;  `(extract-requested-submodule m expected-module)`
                           (eval m))]
                     [(and (pair? expected-module)
                           (not (car expected-module)))
                      ;; shouldn't load from source when `expected-module` starts with #f
                      (void)]
                     [else
                      (unless quiet-load?
                        (log-status "compile: ~a" path))
                      (set! cache-skip-first? #f)
                      (with-handlers ([exn:fail? (lambda (exn)
                                                   (unless quiet-load?
                                                     (log-status "...during ~a..." path))
                                                   (raise exn))])
                        (define s
                          (call-with-input-file*
                           path
                           (lambda (i)
                             (port-count-lines! i)
                             (with-module-reading-parameterization
                                 (lambda ()
                                   (check-module-form
                                    (read-syntax (object-name i) i)
                                    path))))))
                        (cond
                         [(not cache)
                          (eval s)]
                         [else
                          (define cache-layer (make-cache-layer))
                          (define c
                            (parameterize ([current-cache-layer cache-layer])
                              (compile s)))
                          (when time-expand?
                            ;; Re-expanding avoids timing load of required modules
                            (time (expand s)))
                          (cond
                           [(and cache
                                 (not cache-read-only?)
                                 (or (not cache-save-only)
                                     (hash-ref cache-save-only (path->string path) #f)))
                            (cache-compiled! cache path c cache-layer)
                            (loop)]
                           [else (eval c)])]))]))]
                 [else (orig-load path #f)])))

(define orig-resolver (current-module-name-resolver))
(current-module-name-resolver
 (case-lambda
   [(r ns) (orig-resolver r ns)]
   [(r wrt src load?)
    (define p (orig-resolver r wrt src load?))
    (define n (resolved-module-path-name p))
    (when (and (path? n) cache)
      (register-dependency! cache n))
    p]))

(define (apply-to-module proc mod-path)
  (define path (resolved-module-path-name
                (resolve-module-path mod-path #f)))
  (define-values (dir file dir?) (split-path path))
  (parameterize ([current-load-relative-directory dir])
    (proc (call-with-input-file*
           path
           (lambda (i)
             (port-count-lines! i)
             (with-module-reading-parameterization
                 (lambda ()
                   (check-module-form
                    (read-syntax (object-name i) i)
                    path))))))))

(cond
 [expand?
  (pretty-write (syntax->datum (apply-to-module expand startup-module)))]
 [linklets?
  (pretty-write (correlated->datum
                 (datum->correlated
                  (apply-to-module compile startup-module) #f)))]
 [else
  ;; Load and run the requested module
  (parameterize ([current-command-line-arguments (list->vector args)])
    (namespace-require (if submod-name
                           `(submod ,startup-module ,submod-name)
                           startup-module)))])

(when extract?
  ;; Extract a bootstrapping slice of the requested module
  (extract startup-module cache
           #:print-extracted-to print-extracted-to
           #:as-c? extract-to-c?
           #:as-decompiled? extract-to-decompiled?
           #:as-bytecode? extract-to-bytecode?
           #:local-rename? local-rename?
           #:no-global? no-global?
           #:global-ok global-ok
           #:instance-knot-ties instance-knot-ties
           #:primitive-table-directs primitive-table-directs
           #:side-effect-free-modules side-effect-free-modules
           #:disallows disallows))

(when load-file
  (load load-file))

;; ----------------------------------------

(when (or dependencies-file
          makefile-dependencies-file)
  (for ([mod-file (in-list extra-module-dependencies)])
    (define deps (cons mod-file
                       (module-recorded-dependencies mod-file)))
    (for ([dep (in-list deps)])
      (hash-set! dependencies (simplify-path (path->complete-path dep)) #t)))
  ;; Note: `cache` currently misses external dependencies, such as
  ;; `include`d files.
  (for ([dep (in-list (cache->used-paths cache))])
    (hash-set! dependencies (simplify-path dep) #t)))

(when dependencies-file
  (call-with-output-file*
   dependencies-file
   #:exists 'truncate/replace
   (lambda (o)
     (writeln (for/list ([dep (in-hash-keys dependencies)])
                (path->bytes dep))
              o))))

(when makefile-dependencies-file
  (define (quote-if-space s) (if (regexp-match? #rx" " s) (format "\"~a\"" s) s))
  (call-with-output-file*
   makefile-dependencies-file
   #:exists 'truncate/replace
   (lambda (o)
     (fprintf o "~a:" (quote-if-space makefile-dependencies-target))
     (for ([dep (in-hash-keys dependencies)])
       (fprintf o " \\\n  ~a" (quote-if-space dep)))
     (newline o)
     (for ([dep (in-hash-keys dependencies)])
       (fprintf o "\n~a:\n" (quote-if-space dep))))))
