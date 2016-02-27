;; This program reads Racket/GRacket C/C++ source and transforms it
;; to work with precise garbage collection or(!) PalmOS. The source
;; is C-pre-processed first, then run though a `lex'-like lexer,
;; ctok.rkt.
;;
;; It probably won't work for other C/C++ code, because it
;; doesn't bother *parsing* the source. Instead, it relies on
;; various heuristics that work for Racket/GRacket code.
;;
;; There are also some input hacks, such as START_XFORM_SKIP.
;; 
;; Notable assumptions:
;;  No calls of the form (f)(...).
;;  For arrays, records, and non-pointers, pass by address only.
;;  No gc-triggering code in .h files.
;;  No instance vars declared as function pointers without a typedef
;;    for the func ptr type.
;;
;; BUGS: Doesn't check for pointer comparisons where one of the
;;       comparees is a function call. This doesn't happen in
;;       Racket/GRacket (or, because of this bug, shouldn't!).
;;
;;       Passing the address of a pointer is dangerous; make sure
;;       that the pointer is used afterward, otherwise it pointer
;;       might not get updated during GC.
;;
;;       A "return;" can get converted to "{ <something>; return; };",
;;       which can break "if (...) return; else ...".

;; To call for Precise GC:
;;   racket -qr xform.rkt [--setup] [--precompile] [--precompiled <file>] [--notes] [--depends] [--cgc] <cpp> <src> <dest>
;;
;;   Or: Set the XFORM_PRECOMP=yes environment variable to imply --precompile
;;       Set the XFORM_USE_PRECOMP=<file> to imply --precompiled <file>
;;
;; To call for Palm:
;;   racket -qr xform.rkt [--setup] [--notes] [--depends] --palm <cpp> <src> <dest> <mapdest>

;; General code conventions:
;;   e means a list of tokens, often ending in a '|;| token
;;   -e means a reversed list of tokens

(module xform '#%kernel
  (#%require '#%min-stx)

  (define-values (rel-dir)
    (if (string=? "--setup" (vector-ref (current-command-line-arguments) 0))
        (vector-ref (current-command-line-arguments) 1)
        "."))

  (define-values (here-dir)
    (let-values ([(base name dir?)
                  (split-path
                   (resolved-module-path-name
                    (module-path-index-resolve 
                     (syntax-source-module (quote-syntax here)))))])
      (build-path base rel-dir)))

  (if (string=? "--setup"
                (vector-ref (current-command-line-arguments) 0))

      ;; Setup an xform-collects tree for running xform.
      ;; Delete existing xform-collects tree if it's for an old version
      (let retry ()
        (parameterize ([current-directory rel-dir])
          (unless (and (file-exists? "xform-collects/version.rkt")
                       (equal? (version)
                               (with-input-from-file "xform-collects/version.rkt" read))
                       (>= (file-or-directory-modify-seconds (build-path "xform-collects/xform/xform-mod.rkt"))
                           (file-or-directory-modify-seconds (build-path here-dir "xform-mod.rkt"))))
            ;; In case multiple xforms run in parallel, use a lock file
            ;;  so that only one is building.
            (let ([lock-file "XFORM-LOCK"])
              ((call-with-escape-continuation
                (lambda (escape)
                  (parameterize ([uncaught-exception-handler
                                  (lambda (exn)
                                    (escape
                                     (lambda ()
                                       (if (exn:fail:filesystem:exists? exn)
                                           (begin
                                             (printf "Lock file exists: ~a\n"
                                                     (path->complete-path lock-file))
                                             (printf " (If this isn't a parallel make, then delete it.)\n")
                                             (printf " Waiting until the lock file disappears...\n")
                                             (let loop ()
					       (flush-output)
                                               (sleep 0.1)
                                               (if (file-exists? lock-file)
                                                   (loop)
                                                   (printf " ... continuing\n")))
                                             (retry))
                                           (raise exn)))))])
                    (dynamic-wind
                        (lambda ()
                          (close-output-port (open-output-file lock-file 'error)))
                        (lambda ()
                          (namespace-require 'racket/base)
                          (load (build-path here-dir "setup.rkt"))
                          void)
                        (lambda ()
                          (delete-file lock-file))))))))))

        (use-compiled-file-paths '("compiled"))
        
        (current-library-collection-paths (list (build-path (build-path (current-directory) rel-dir) "xform-collects")))

        (let ([ns (make-empty-namespace)])
          (dynamic-require ''#%builtin #f)
          (namespace-attach-module (current-namespace) ''#%builtin ns)
          (current-namespace ns))
        
        (error-print-width 100)

        (dynamic-require 'xform/xform-mod #f))

      ;; Otherwise, we assume that it's ok to use the collects
      (dynamic-require (build-path here-dir
                                   "xform-mod.rkt")
                       #f)))
