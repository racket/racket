#lang racket/base
(#%declare #:unsafe-cross-phase-persistent)
(require "dirs.rkt" "path-relativize.rkt")

(provide path->main-collects-relative
         main-collects-relative->path)

(define-values (path->main-collects-relative
                main-collects-relative->path)
  (make-relativize find-collects-dir
                   'collects
                   'path->main-collects-relative
                   'main-collects-relative->path))
