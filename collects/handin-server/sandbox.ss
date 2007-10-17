(module sandbox mzscheme
  (require (lib "sandbox.ss"))
  (provide (all-from (lib "sandbox.ss")))

  ;; no input/output
  (sandbox-input        #f)
  (sandbox-output       #f)
  (sandbox-error-output #f)

  ;; no limits -- the handin server uses per-session limits
  (sandbox-eval-limits #f)

  ;; share these with evaluators
  (sandbox-namespace-specs
   (let ([specs (sandbox-namespace-specs)])
     `(,(car specs)
       ,@(cdr specs)
       (lib "posn.ss" "lang")
       ,@(if mred? '((lib "cache-image-snip.ss" "mrlib")) '()))))

  ;; local overrides
  (sandbox-override-collection-paths
   (cons (build-path (collection-path "handin-server") "overridden-collects")
         (sandbox-override-collection-paths)))

  )
