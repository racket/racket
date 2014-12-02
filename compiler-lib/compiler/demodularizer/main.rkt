#lang racket/base
(require compiler/cm
         compiler/zo-marshal
         "alpha.rkt"
         "gc-toplevels.rkt"
         "merge.rkt"
         "module.rkt"
         "mpi.rkt"
         "nodep.rkt"
         "replace-modidx.rkt")

(provide current-excluded-modules
         garbage-collect-toplevels-enabled
         demodularize)

(define garbage-collect-toplevels-enabled (make-parameter #f))

(define logger (make-logger 'demodularizer (current-logger)))

(define (demodularize file-to-batch [output-file #f])
  (parameterize ([current-logger logger])
    (define-values (base name must-be-dir?) (split-path file-to-batch))
    (when must-be-dir?
      (error 'demodularize "Cannot run on directory: ~a" file-to-batch))
    (unless (file-exists? file-to-batch)
      (error 'demodularize "File does not exist: ~a" file-to-batch))
    
    ;; Compile
    (log-info "Compiling module")
    (parameterize ([current-namespace (make-base-empty-namespace)])
      (managed-compile-zo file-to-batch))
    
    (define merged-zo-path 
      (or output-file
          (path-add-suffix file-to-batch #"_merged.zo")))
    
    ;; Transformations
    (define path-cache (make-hasheq))
    
    (log-info "Removing dependencies")
    (define-values (batch-nodep top-lang-info top-self-modidx get-modvar-rewrite)
      (parameterize ([MODULE-PATHS path-cache])
        (nodep-file file-to-batch)))
    
    (log-info "Merging modules")
    (define batch-merge
      (parameterize ([MODULE-PATHS path-cache])
        (merge-compilation-top get-modvar-rewrite batch-nodep)))
    
    (define batch-gcd
      (if (garbage-collect-toplevels-enabled)
          (begin
            (log-info "GC-ing top-levels")
            (gc-toplevels batch-merge))
          batch-merge))
    
    (log-info "Alpha-varying top-levels")
    (define batch-alpha
      (alpha-vary-ctop batch-gcd))
    
    (log-info "Replacing self-modidx")
    (define batch-replace-modidx
      (replace-modidx batch-alpha top-self-modidx))
    
    (define batch-modname
      (string->symbol (regexp-replace #rx"\\.zo$" (path->string merged-zo-path) "")))
    (log-info (format "Modularizing into ~a" batch-modname))
    (define batch-mod
      (wrap-in-kernel-module batch-modname batch-modname top-lang-info top-self-modidx batch-replace-modidx))
    
    (log-info "Writing merged zo")
    (void
     (with-output-to-file 
         merged-zo-path
       (lambda ()
         (zo-marshal-to batch-mod (current-output-port)))
       #:exists 'replace))))
