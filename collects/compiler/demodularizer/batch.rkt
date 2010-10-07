#lang racket
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
         "util.rkt"
         "nodep.rkt"
         "merge.rkt"
         "gc-toplevels.rkt"
         "alpha.rkt"
         "module.rkt"
         compiler/decompile
         compiler/zo-marshal
         racket/set)

(define excluded-modules (make-parameter (set)))
(define file-to-batch
  (command-line #:program "batch" 
                #:multi
                [("-e" "--exclude-modules") mod
                                            "Exclude a module from being batched"
                                            (excluded-modules (set-add (excluded-modules) mod))]
                #:args (filename) filename))

(define-values (base name dir?) (split-path file-to-batch))
(when (or (eq? base #f) dir?)
  (error 'batch "Cannot run on directory"))


;; Compile 
#;(log-debug "Removing existing zo file~n")
#;(define compiled-zo-path (build-compiled-path base (path-add-suffix name #".zo")))

#;(when (file-exists? compiled-zo-path)
    (delete-file compiled-zo-path))

(log-debug "Compiling module~n")
(void (system* (find-executable-path "raco") "make" file-to-batch)) 


(define merged-source-path (path-add-suffix file-to-batch #".merged.rkt"))
(define merged-struct-path (path-add-suffix file-to-batch #".mergeds.rkt"))
(define-values (merged-source-base merged-source-name _1) (split-path merged-source-path))
(define merged-zo-path (build-compiled-path merged-source-base (path-add-suffix merged-source-name #".zo")))

;; Transformations
(log-debug "Removing dependencies~n")
(define-values (batch-nodep top-lang-info top-self-modidx)
  (nodep-file file-to-batch (excluded-modules)))

(log-debug "Merging modules~n")
(define batch-merge
  (merge-compilation-top batch-nodep))

(log-debug "GC-ing top-levels~n")
(define batch-gcd
  batch-merge
  #;(gc-toplevels batch-merge))

(log-debug "Alpha-varying top-levels~n")
(define batch-alpha
  (alpha-vary-ctop batch-gcd))

(define batch-modname
  (string->symbol (regexp-replace #rx"\\.rkt$" (path->string merged-source-name) "")))
(log-debug (format "Modularizing into ~a~n" batch-modname))
(define batch-mod
  (wrap-in-kernel-module batch-modname batch-modname top-lang-info top-self-modidx batch-alpha))

;; Output
(define batch-final batch-mod)

(log-debug "Writing merged source~n")
(with-output-to-file
    merged-source-path
  (lambda ()
    (pretty-print (decompile batch-final)))
  #:exists 'replace)

(log-debug "Writing merged struct~n")
(with-output-to-file
    merged-struct-path
  (lambda ()
    (pretty-write batch-final))
  #:exists 'replace)

(log-debug "Writing merged zo~n")
(void
 (with-output-to-file 
     merged-zo-path
   (lambda ()
     (write-bytes (zo-marshal batch-final)))
   #:exists 'replace))




