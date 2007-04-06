(module sandbox mzscheme
  (require (lib "sandbox.ss"))
  (provide (all-from (lib "sandbox.ss")))

  (sandbox-namespace-specs
   (let ([specs (sandbox-namespace-specs)])
     `(,(car specs)
       ,@(cdr specs)
       (lib "posn.ss" "lang")
       ,@(if mred? '((lib "cache-image-snip.ss" "mrlib")) '()))))

  (sandbox-override-collection-paths
   (cons (build-path (collection-path "handin-server") "overridden-collects")
         (sandbox-override-collection-paths)))

  )
