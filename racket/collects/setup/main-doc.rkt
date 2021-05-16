#lang s-exp racket/base
(require "dirs.rkt"
         "path-relativize.rkt")

(provide path->main-doc-relative
         main-doc-relative->path)

(define-values (path->main-doc-relative
                main-doc-relative->path)
  (make-relativize (lambda ()
                     (define d (find-doc-dir))
                     (define extras (get-doc-extra-search-dirs))
                     (if d (cons d extras) extras))
                   'doc
                   'path->main-doc-relative
                   'main-doc-relative->path))
