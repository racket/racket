#lang scheme/base
(require "checker.ss"
         "specs.ss"
         "dist-specs.ss")

(parameterize ([*specs* #f]
               [current-namespace (namespace-anchor->namespace checker-namespace-anchor)])
  (define (/-ify x)
    (regexp-replace #rx"/?$" (if (path? x) (path->string x) x) "/"))
  (define plt/      (/-ify (simplify-path (build-path (collection-path "scheme") 'up 'up))))
  (define plt-base/ (/-ify (simplify-path (build-path plt/ 'up) #f)))
  (define plt/-name (let-values ([(base name dir?) (split-path plt/)])
                      (path-element->string name)))

  (register-macros!)

  (register-specs! *specs*)

  (register-spec! 'verify! verify!)
  (register-spec! 'distribute! void)

  (set-plt-tree! plt-base/ plt/-name null)

  (set-bin-files-delayed-lists! 
   ;; FIXME: hard-wired list of binary-specific files
   '(("plt/collects/sgl/compiled/gl-info_ss.zo")))

  (expand-spec 'distributions)

  (void))
