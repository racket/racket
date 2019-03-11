
;; Because Setup PLT is used to rebuild .zos, we may need to turn off
;; the use of compiled code or install cm before we do anything. This
;; startup stub parses the command line and either disables .zos or
;; installs cm for loading Setup PLT.

;; Note that this file is listed in "info.rkt" so that it never gets a
;; .zo file. Do not `require' this module from anywhere, not even 
;; `for-label', otherwise it could get a .zo anyway.

;; Also, do not `require' any module that is compiled. That constraint
;; essentially restricts this module to `require's of '#%... modules.

(module main '#%kernel
  (#%require '#%utils ; for find-main-collects
             '#%paramz
             ;; Need to make sure they're here:
             '#%builtin
             (for-syntax '#%kernel))

  (module test '#%kernel)
  
  ;; ----------------------------------------
  ;; Some minimal syntax extensions to '#%kernel
  
  (define-syntaxes (parameterize)
    (lambda (stx)
      (let-values ([(s) (cdr (syntax->list stx))])
        (let-values ([(bindings) (apply append
                                        (map syntax->list (syntax->list (car s))))])
          (datum->syntax 
           (quote-syntax here)
           (list 'with-continuation-mark
                 'parameterization-key
                 (list* 'extend-parameterization
                        '(continuation-mark-set-first #f parameterization-key)
                        bindings)
                 (list* 'let-values ()
                        (cdr s))))))))

  (define-syntaxes (and)
    (lambda (stx)
      (let-values ([(s) (cdr (syntax->list stx))])
        (if (null? s)
            (quote-syntax #t)
            (if (null? (cdr s))
                (car s)
                (datum->syntax (quote-syntax here)
                               (list 'if (car s) (cons 'and (cdr s)) #f)))))))

  (define-syntaxes (or)
    (lambda (stx)
      (let-values ([(s) (cdr (syntax->list stx))])
        (if (null? s)
            (quote-syntax #f)
            (if (null? (cdr s))
                (car s)
                (datum->syntax (quote-syntax here)
                               (list 'let-values (list (list (list 'x)
                                                             (car s)))
                                     (list 'if 'x 'x (cons 'or (cdr s))))))))))

  (define-syntaxes (let)
    (lambda (stx)
      (let-values ([(s) (cdr (syntax->list stx))])
        (datum->syntax 
         (quote-syntax here)
         (if (symbol? (syntax-e (car s)))
             (let-values ([(clauses)
                           (map (lambda (c)
                                  (syntax->list c))
                                (syntax->list (cadr s)))])
               (list 'letrec-values (list (list (list (car s))
                                                (list* 'lambda
                                                       (map car clauses)
                                                       (cddr s))))
                     (cons (car s) (map cadr clauses))))
             (list* 'let-values (map (lambda (c)
                                       (let-values ([(c) (syntax->list c)])
                                         (cons (list (car c))
                                               (cdr c))))
                                     (syntax->list (car s)))
                    (cdr s)))))))
  
  ;; ----------------------------------------

  (if (file-stream-port? (current-output-port))
      (file-stream-buffer-mode (current-output-port) 'line)
      (void))

  (define-values (make-kernel-namespace)
    (lambda ()
      (let-values ([(ns) (make-empty-namespace)]
                   [(cns) (current-namespace)])
        (namespace-attach-module cns ''#%builtin ns)
        ns)))

  (define-values (short-name long-names raco?)
    ;; Load the name module without using .zos, and in its own namespace to 
    ;;  avoid polluting the cm-managed namespace later
    (parameterize ([use-compiled-file-paths null]
                   [current-namespace (make-kernel-namespace)])
      ((dynamic-require 'setup/private/command-name 'get-names))))

  (define-values (member)
    (lambda (a l)
      (if (null? l)
          #f
          (if (equal? a (car l))
              l
              (member a (cdr l))))))

  (define-values (go-module) 'setup/setup-go)
  (define-values (print-loading-sources?) #f)

  ;; Poor-man's processing of the command-line flags to drop strings
  ;; that will not be parsed as flags by "parse-cmdline.rkt". We don't
  ;; want to load "parse-cmdline.rkt" because it takes a long time with
  ;; bytecode files disabled, and we're not yet sure whether to trust
  ;; bytecode files that do exist.
  (define-values (filter-flags)
    (lambda (queued-flags flags)
      (let ([flags (if (pair? queued-flags)
                       (cons (car queued-flags) flags)
                       flags)]
            [queued-flags (if (pair? queued-flags)
                              (cdr queued-flags)
                              '())])
        (if (or (null? flags)
                (not (regexp-match? #rx"^-" (car flags)))
                (member (car flags)
                        ;; Flags that end flag processing:
                        '("-l" "--pkgs" "--")))
            queued-flags
            (if (equal? "-P" (car flags))
                (if ((length flags) . > . 5)
                    (filter-flags queued-flags (list-tail flags 5))
                    queued-flags)
                (if (member (car flags)
                            ;; Flags that take 1 argument:
                            '("--mode" "--doc-pdf"
                              "-j" "--jobs" "--workers"
                              "--error-in" "--error-out"))
                    (if (pair? (cdr flags))
                        (filter-flags queued-flags (cddr flags))
                        queued-flags)
                    (if (or (equal? "--boot" (car flags))
                            (equal? "--chain" (car flags)))
                        ;; Record an alternate boot module and [additional] compiled-file root
                        (if (and (pair? (cdr flags))
                                 (pair? (cddr flags)))
                            (begin
                              (set! go-module (list 'file (cadr flags)))
                              (set! print-loading-sources? #t)
                              (let ([root (path->complete-path (caddr flags))])
                                (current-compiled-file-roots
                                 (if (equal? "--boot" (car flags))
                                     (list root)
                                     (cons root (current-compiled-file-roots)))))
                              (cons (car flags)
                                    (filter-flags queued-flags (cddr flags))))
                            queued-flags)
                        ;; Check for combined flags and split them apart:
                        (if (regexp-match? #rx"^-([^-].+)" (car flags))
                            (filter-flags (append
                                           (map (lambda (c)
                                                  (string #\- c))
                                                (cdr (string->list (car flags))))
                                           queued-flags)
                                          (cdr flags))
                            ;; A flag with no argument:
                            (cons (car flags)
                                  (filter-flags queued-flags (cdr flags)))))))))))

  (define-values (flags) (filter-flags '() (vector->list (current-command-line-arguments))))

  ;; Checks whether a flag is present:
  (define-values (on?)
    (lambda (flag-name)
      (member flag-name flags)))

  (define-values (print-bootstrapping)
    (lambda (why)
      (fprintf (current-output-port)
               "~a: bootstrapping from source...\n ~a\n"
               short-name
               why)))

  (define-values (main-collects-relative->path)
    (let ([main-collects #f])
      (lambda (p)
        (if main-collects
            (void)
            (set! main-collects (find-main-collects)))
        (if (and (pair? p)
                 (eq? 'collects (car p)))
            (apply build-path main-collects
                   (map bytes->path (cdr p)))
            p))))

  (define-values (original-compiled-file-paths) (use-compiled-file-paths))

  (if (or (on? "--clean")
          (on? "-c")
          (on? "--no-zo")
          (on? "-n"))
      ;; Don't use .zos, in case they're out of date, and don't load
      ;;  cm:
      (if (or (on? "--clean")
              (on? "-c"))
          (begin
            (use-compiled-file-paths null)
            (print-bootstrapping "triggered by command-line `--clean` or `-c`"))
          (void))

      ;; Load the cm instance to be installed while loading Setup PLT.
      ;; This has to be dynamic, so we get a chance to turn off compiled
      ;;  file loading, and so it can be in a separate namespace.
      (let-values ([(mk trust-zos managed-recompile-only)
		    ;; Load cm.rkt into its own namespace, so that cm compiles
		    ;;  itself and its required modules in the right order
		    ;;  (i.e., when some module requires cm or one of its
		    ;;  required modules)
		    ;; Since cm.rkt pulls in quite a lot of code itself, we
		    ;;  would like to load using .zo files. But if we discover
		    ;;  any date mismatch in the loading process, abort and
		    ;;  try again without .zo files. If .zo files are newer
		    ;;  than .rkt files but a required file is newer than its
		    ;;  requiring file, we won't notice, but that
		    ;;  shouldn't happen for a reasonably maintained
		    ;;  tree, and there's always --clean to turn this
		    ;;  off. If an .so file is used, we give up using
		    ;;  compiled files.
		    (let loop ([skip-zo/reason (and (null? (use-compiled-file-paths))
                                                    "empty use-compiled-file-paths")])
		      (if skip-zo/reason
                          (print-bootstrapping skip-zo/reason)
                          (void))
		      ((call-with-escape-continuation
                        (lambda (escape)
			 ;; Create a new namespace, and also install load handlers
			 ;;  to check file dates, if necessary.
			 (parameterize ([current-namespace (make-kernel-namespace)]
					[use-compiled-file-paths 
					 (if skip-zo/reason
					     null
					     (use-compiled-file-paths))]
					[current-load 
					 (let ([orig-load (current-load)])
					   (if skip-zo/reason
                                               (if print-loading-sources?
                                                   (lambda (path modname)
                                                     (log-message (current-logger) 'info 'compiler/cm (format "loading ~a" path))
                                                     (orig-load path modname))
                                                   orig-load)
					       (lambda (path modname)
						 (if (regexp-match? #rx#"[.]zo$" (path->bytes path))
						     ;; It's a .zo:
                                                     (begin0
                                                      (orig-load path modname)
                                                      ;; Force loading of all dependencies, which ensures
                                                      ;; a rebuild if a #lang reader changes. (Otherwise,
                                                      ;; the dependencies should be loaded already.)
                                                      ;; We do not currently support "external" dependencies
                                                      ;; (via cm-accomplice) during bootstrap.
                                                      (let ([deps (with-input-from-file 
                                                                      (path-replace-extension path #".dep")
                                                                    read)])
                                                        (for-each (lambda (dep)
                                                                    (let ([dep
                                                                           (if (and (pair? dep)
                                                                                    (eq? (car dep) 'indirect))
                                                                               (cdr dep)
                                                                               dep)])
                                                                      (if (and (pair? dep)
                                                                               (eq? (car dep) 'ext))
                                                                          (void)
                                                                          (dynamic-require (main-collects-relative->path dep) #f))))
                                                                  (cdddr deps))))
						     ;; Not a .zo! Don't use .zo files at all...
						     (escape (lambda ()
							       ;; Try again without .zo
							       (loop (format "triggered by use of non-\".zo\" file\n  path: ~a" path))))))))]
					[current-load-extension 
					 (if skip-zo/reason
					     (current-load-extension)
					     (lambda (path modname)
					       (escape (lambda ()
							 ;; Try again without .zo
							 (loop "triggered by loading an extension")))))])
		           ;; Other things could go wrong, such as a version mismatch.
		           ;; If something goes wrong, of course, give up on .zo files.
                           (parameterize ([uncaught-exception-handler
                                           (lambda (exn)
                                             (if (exn:break? exn) (exit 1) (void))
                                             (if skip-zo/reason
                                                 (escape
                                                  (lambda () (raise exn)))
                                                 (escape
                                                  (lambda () (loop (if (exn:fail? exn)
                                                                       (exn-message exn)
                                                                       (format "uncaught exn: ~s" exn)))))))])
			     ;; Here's the main dynamic load of "cm.rkt":
			     (let ([mk
				    (dynamic-require 'compiler/private/cm-minimal
						     'make-compilation-manager-load/use-compiled-handler)]
				   [trust-zos
				    (dynamic-require 'compiler/private/cm-minimal 'trust-existing-zos)]
                                   [managed-recompile-only
                                    (dynamic-require 'compiler/private/cm-minimal 'managed-recompile-only)])
			       ;; Return the extracted functions:
			       (lambda () (values mk trust-zos managed-recompile-only)))))))))])
	(if (on? "--trust-zos")
            (trust-zos #t)
            (void))
        (if (on? "--recompile-only")
            (managed-recompile-only #t)
            (void))
	(current-load/use-compiled (mk))))

  ;; This has to be dynamic, so we get a chance to turn off
  ;;  .zo use and turn on the compilation manager.
  ((dynamic-require go-module 'go) original-compiled-file-paths))
