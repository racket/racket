#lang racket/base
(require compiler/depend)

(cond
  [(equal? (current-command-line-arguments)
           '#("--get-config"))
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
      ;; FIXME:
      (config-dependencies ())))]
  [(and (= 2 (vector-length (current-command-line-arguments)))
        (equal? (vector-ref (current-command-line-arguments) 0)
                "--dependencies"))
   (for-each
    (lambda (p) (writeln (path->bytes p)))
    (module-recorded-dependencies (vector-ref (current-command-line-arguments) 1)))]
  [else
   (error 'zuo-help "unhandled arguments" (current-command-line-arguments))])
