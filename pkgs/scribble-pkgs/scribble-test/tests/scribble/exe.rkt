#lang racket/base
(require racket/file
         compiler/embed
         racket/system)

;; Check that `scribble/manual` can be embedded in an executable

(define src (make-temporary-file))
(define exe (make-temporary-file))

(call-with-output-file src
  #:exists 'truncate
  (lambda (o)
    (write '(module m racket/base
              (require scribble/manual))
           o)))

(define mod-sym (string->symbol
                 (format "~a"
                         (let-values ([(base name dir?)
                                       (split-path src)])
                           (path->bytes (path-replace-suffix name #""))))))

(create-embedding-executable exe
                             #:cmdline '("-U")
                             #:collects-path null
                             #:modules `((#f ,src))
                             #:configure-via-first-module? #t
                             #:literal-expression
                             (parameterize ([current-namespace (make-base-namespace)])
                               (compile `(namespace-require '',mod-sym))))

(system* exe)
