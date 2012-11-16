#lang racket/base

(provide (struct-out cc))

(define-struct cc
  (collection path name info omit-root info-root info-path info-path-mode shadowing-policy)
  #:inspector #f)
