
;; Because Setup PLT is used to rebuild .zos, we may need to turn off
;; the use of compiled code or install cm before we do anything. This
;; startup stub parses the command line and either disables .zos or
;; installs cm for loading Setup PLT.

;; Note that this file is listed in "info.rkt" so that it never gets a
;; .zo file. Do not `require' this module from anywhere, not even 
;; `for-label', otherwise it could get a .zo anyway.

;; Also, do not `require' any module that is compiled. That constraint
;; essentially restrcts this module to `require's of '#%... modules.

(module main '#%kernel
  (#%require '#%min-stx
             '#%utils ; for find-main-collects
             ;; Need to make sure they're here:
             '#%builtin)

  (when (file-stream-port? (current-output-port))
    (file-stream-buffer-mode (current-output-port) 'line))

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

  ;; Poor-man's processing of the command-line flags to drop strings
  ;; that will not be parsed as flags by "parse-cmdline.rkt". We don't
  ;; want to load "parse-cmdline.rkt" because it takes a long time with
  ;; bytecode files disabled, and we're not yet sure whether to trust
  ;; bytecode files that do exist.
  (define-values (filter-flags)
    (lambda (flags)
      (if (or (null? flags)
              (not (regexp-match? #rx"^-" (car flags)))
              (equal? "-l" (car flags)))
          null
          (if (equal? "-P" (car flags))
              (if ((length flags) . > . 5)
                  (filter-flags (list-tail flags 5))
                  null)
              (if (or (equal? "--mode" (car flags))
                      (equal? "--doc-pdf" (car flags)))
                  (if (pair? (cdr flags))
                      (filter-flags (cddr flags))
                      null)
                  (cons (car flags) (filter-flags (cdr flags))))))))

  (define-values (flags) (filter-flags (vector->list (current-command-line-arguments))))

  (define-values (member)
    (lambda (a l)
      (if (null? l)
          #f
          (if (equal? a (car l))
              l
              (member a (cdr l))))))

  ;; Checks whether a flag is present:
  (define-values (on?)
    (lambda (flag-name)
      (member flag-name flags)))

  (define-values (print-bootstrapping)
    (lambda ()
      (fprintf (current-output-port) "~a: bootstrapping from source...\n" short-name)))

  (define-values (main-collects-relative->path)
    (let ([main-collects #f])
      (lambda (p)
        (unless main-collects
          (set! main-collects (find-main-collects)))
        (if (and (pair? p)
                 (eq? 'collects (car p)))
            (apply build-path main-collects
                   (map bytes->path (cdr p)))
            p))))

  (if (or (on? "--clean")
          (on? "-c")
	  (on? "--no-zo")
          (on? "-n"))
      ;; Don't use .zos, in case they're out of date, and don't load
      ;;  cm:
      (when (or (on? "--clean")
                (on? "-c"))
	(use-compiled-file-paths null)
	(print-bootstrapping))
  
      ;; Load the cm instance to be installed while loading Setup PLT.
      ;; This has to be dynamic, so we get a chance to turn off compiled
      ;;  file loading, and so it can be in a separate namespace.
      (let-values ([(mk trust-zos)
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
		    (let loop ([skip-zo? (null? (use-compiled-file-paths))])
		      (when skip-zo?
			(print-bootstrapping))
		      ((call/ec 
                        (lambda (escape)
			 ;; Create a new namespace, and also install load handlers
			 ;;  to check file dates, if necessary.
			 (parameterize ([current-namespace (make-kernel-namespace)]
					[use-compiled-file-paths 
					 (if skip-zo?
					     null
					     (use-compiled-file-paths))]
					[current-load 
					 (let ([orig-load (current-load)])
					   (if skip-zo?
					       orig-load
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
                                                                      (bytes->path (regexp-replace #"[.]zo$" (path->bytes path) #".dep"))
                                                                    read)])
                                                        (for-each (lambda (dep)
                                                                    (unless (and (pair? dep)
                                                                                 (eq? (car dep) 'ext))
                                                                      (dynamic-require (main-collects-relative->path dep) #f)))
                                                                  (cddr deps))))
						     ;; Not a .zo! Don't use .zo files at all...
						     (escape (lambda ()
							       ;; Try again without .zo
							       (loop #t)))))))]
					[current-load-extension 
					 (if skip-zo?
					     (current-load-extension)
					     (lambda (path modname)
					       (escape (lambda ()
							 ;; Try again without .zo
							 (loop #t)))))])
		           ;; Other things could go wrong, such as a version mismatch.
		           ;; If something goes wrong, of course, give up on .zo files.
                           (parameterize ([uncaught-exception-handler
                                           (lambda (exn)
                                             (when (exn:break? exn) (exit 1))
                                             (if skip-zo?
                                                 (escape
                                                  (lambda () (raise exn)))
                                                 (escape
                                                  (lambda () (loop #t)))))])
			     ;; Here's the main dynamic load of "cm.rkt":
			     (let ([mk
				    (dynamic-require 'compiler/cm
						     'make-compilation-manager-load/use-compiled-handler)]
				   [trust-zos
				    (dynamic-require 'compiler/cm 'trust-existing-zos)])
			       ;; Return the two extracted functions:
			       (lambda () (values mk trust-zos)))))))))])
	(when (on? "--trust-zos")
	  (trust-zos #t))
	(current-load/use-compiled (mk))))

  ;; This has to be dynamic, so we get a chance to turn off
  ;;  .zo use and turn on the compilation manager.
  (dynamic-require 'setup/setup-go #f))
