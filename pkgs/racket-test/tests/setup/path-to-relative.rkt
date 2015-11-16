#lang racket/base
(require setup/path-to-relative
         rackunit
         setup/dirs
         racket/path)

(check-equal? "<collects>/"
              (path->relative-string/library (find-collects-dir)))
(check-equal? "<collects>/racket"
              (path->relative-string/library (path-only (collection-file-path "base.rkt" "racket"))))
(check-equal? "<collects>/racket/base.rkt"
              (path->relative-string/library (collection-file-path "base.rkt" "racket")))
(check-equal? "<collects>/racket/base.rkt"
              (path->relative-string/library (normal-case-path (collection-file-path "base.rkt" "racket"))))
