#lang scheme/base
(require scheme/sandbox)
(provide (all-from-out scheme/sandbox))

;; no input/output
(sandbox-input        #f)
(sandbox-output       #f)
(sandbox-error-output #f)

;; no limits -- the handin server uses per-session limits
(sandbox-memory-limit #f)
(sandbox-eval-limits #f)

;; share these with evaluators
(sandbox-namespace-specs
 (let ([specs (sandbox-namespace-specs)])
   `(,(car specs)
     ,@(cdr specs)
     lang/posn
     ,@(if gui? '(mrlib/cache-image-snip) '()))))

;; local overrides
(sandbox-override-collection-paths
 (cons (build-path (collection-path "handin-server") "overridden-collects")
       (sandbox-override-collection-paths)))
