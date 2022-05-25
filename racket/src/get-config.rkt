#lang racket/base
(require compiler/depend)

(for-each
 writeln
 `((version ,(version))
   (machine ,(current-compile-target-machine))
   (compiled-dir ,(path->bytes (car (use-compiled-file-paths))))
   (collects ,(map path->bytes
                   (current-library-collection-paths)))
   (links ,(map (lambda (p)
                  (cond
                    [(path? p) (path->bytes p)]
                    [(not p) #f]
                    [else (error "supported collection-links configuration" (current-library-collection-links))]))
                (current-library-collection-links)))
   (config-dependencies ())))
