#lang s-exp racket/base
(require "dirs.rkt" "path-relativize.rkt")

(provide path->main-doc-relative
         main-doc-relative->path)

(define-values (path->main-doc-relative
                main-doc-relative->path)
  (make-relativize find-doc-dir
                   'doc
                   'path->main-doc-relative
                   'main-doc-relative->path))
