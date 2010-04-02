#lang scheme/base

(provide get-info)

(define get-info
  (lambda (key def get-default)
    (case key
      [(configure-runtime)
       '#(racket/private/runtime configure #f)]
      [else
       (get-default key def)])))

