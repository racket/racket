#lang racket  

(provide (struct-out cc))

(define-struct cc
  (collection path name info root-dir info-path shadowing-policy)
  #:inspector #f)

