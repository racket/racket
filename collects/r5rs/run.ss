#lang scheme/base
(require scheme/cmdline)

(define slow (make-parameter #f))

(define-values (main args)
  (command-line
   #:once-each
   [("--no-prim") "allow redefinition of primitives (implies worse performance)"
    (slow #t)]
   #:handlers
   (case-lambda
    [(x) (values #f null)]
    [(x file . args) (values file args)])
   '("file" "arg")))

(if (slow)
    (namespace-require/copy 'r5rs/init)
    (namespace-require/constant 'r5rs/init))

(current-command-line-arguments (apply vector-immutable args))
(if main
    ;; File load mode:
    (load main)
    ;; REPL mode:
    (begin
      (display (banner))
      (printf "R5RS legacy support loaded\n")
      ;; Load .pltr5rsrc
      (let-values ([(base name dir?) (split-path (find-system-path 'init-file))])
        (let ([f (build-path base (bytes->path-element
                                   (regexp-replace #rx#"mzscheme"
                                                   (path-element->bytes name)
                                                   #"pltr5rs")))])
          (when (file-exists? f)
            (load f))))
      (read-eval-print-loop)))
