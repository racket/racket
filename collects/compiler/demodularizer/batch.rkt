#lang racket/base

#|
Here's the idea:

- Take a module's bytecode
- Recursively get all the bytecode for modules that the target requires
- After reading it, prune everything that isn't at phase 0 (the runtime phase)

- Now that we have all the modules, the next step is to merge them into a single
  module
-- Although actually we collapse them into the top-level, not a module
- To do that, we iterate through all the modules doing two things as we go:
-- Incrementing all the global variable references by all the references in all
   the modules
--- So if A has 5, then B's start at index 5 and so on
-- Replacing module variable references with the actual global variables
   corresponding to those variables
--- So if A's variable 'x' is in global slot 4, then if B refers to it, it 
    directly uses slot 4, rather than a module-variable slot

- At that point we have all the module code in a single top-level, but many
  toplevels won't be used because a library function isn't really used
- So, we do a "garbage collection" on elements of the prefix
- First, we create a dependency graph of all toplevels and the initial scope
- Then, we do a DFS on the initial scope and keep all those toplevels, throwing
  away the construction of everything else
  [XXX: This may be broken because of side-effects.]

- Now we have a small amount code, but because we want to go back to source,
  we need to fix it up a bit; because different modules may've used the same
  names
- So, we do alpha-renaming, but it's easy because names are only used in the
  compilation-top prefix structure

[TODO]

- Next, we decompile
- Then, it will pay to do dead code elimination and inlining, etc.
|#

(require racket/pretty
         racket/system
         racket/cmdline
         "mpi.rkt"
         "util.rkt"
         "nodep.rkt"
         "merge.rkt"
         "gc-toplevels.rkt"
         "alpha.rkt"
         "module.rkt"
         "replace-modidx.rkt"
         compiler/decompile
         compiler/zo-marshal
         racket/set
         raco/command-name)

(define (main file-to-batch output-file)
  (define-values (base name dir?) (split-path file-to-batch))
  (when (or (eq? base #f) dir?)
    (error 'batch "Cannot run on directory"))
  
  ;; Compile 
  
  (log-info "Compiling module")
  (void (system* (find-executable-path "raco") "make" file-to-batch)) 
  
  (define merged-zo-path 
    (or output-file
        (path-add-suffix file-to-batch #"_merged.zo")))
  
  ;; Transformations
  (define path-cache (make-hash))
  
  (log-info "Removing dependencies")
  (define-values (batch-nodep top-lang-info top-self-modidx get-modvar-rewrite)
    (parameterize ([MODULE-PATHS path-cache])
      (nodep-file file-to-batch)))
  
  (log-info "Merging modules")
  (define batch-merge
    (parameterize ([MODULE-PATHS path-cache])
      (merge-compilation-top get-modvar-rewrite batch-nodep)))
  
  ; Not doing this for now
  ;(log-info "GC-ing top-levels")
  (define batch-gcd
    batch-merge
    #;(gc-toplevels batch-merge))
  
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
     #:exists 'replace)))


(let ()
  (define output-file (make-parameter #f))
  (command-line #:program (short-program+command-name)
                #:multi
                [("-e" "--exclude-modules") path "Exclude <path> from flattening"
                 (current-excluded-modules (set-add (current-excluded-modules) path))]
                #:once-each
                [("-o") dest-filename "Write output as <dest-filename>"
                 (output-file (string->path dest-filename))]
                #:args (filename) 
                (main filename (output-file))))
