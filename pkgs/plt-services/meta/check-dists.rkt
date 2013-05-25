#lang scheme/base
(require "checker.rkt"
         "specs.rkt"
         "dist-specs.rkt")

(parameterize ([*specs* #f]
               [current-namespace (namespace-anchor->namespace checker-namespace-anchor)])
  (define (/-ify x)
    (regexp-replace #rx"/?$" (if (path? x) (path->string x) x) "/"))
  (define racket/
    (/-ify (simplify-path (build-path (collection-path "scheme") 'up 'up))))
  (define racket-base/
    (/-ify (simplify-path (build-path racket/ 'up) #f)))
  (define racket/-name
    (let-values ([(base name dir?) (split-path racket/)])
      (path-element->string name)))

  (register-macros!)

  (register-specs! *specs*)

  (register-spec! 'verify! verify!)
  (register-spec! 'distribute! void)

  (set-racket-tree! racket/ racket-base/ racket/-name null)

  (set-bin-files-delayed-lists!
   ;; FIXME: hard-wired list of binary-specific files;
   ;; we assume there are none. This value is a list of
   ;; lists, where a given file must appear in every list
   ;; to be ok for the distribution.
   '(()))

  (expand-spec 'distributions)

  (void))
