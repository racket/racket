#lang racket/base
(require racket/sandbox)
(provide (all-from-out racket/sandbox))

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
(require racket/runtime-path)
(define-runtime-path overrides "overridden-collects")
(sandbox-override-collection-paths
 (cons overrides (sandbox-override-collection-paths)))
