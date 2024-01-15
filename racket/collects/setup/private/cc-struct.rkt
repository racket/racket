#lang racket/base

(require racket/promise)

(provide (struct-out cc) cc-name)

(define-struct cc
  (collection path name* info parent-cc omit-root info-root info-path info-path-mode shadowing-policy main?)
  #:inspector #f)

(define (cc-name v) (force (cc-name* v)))
