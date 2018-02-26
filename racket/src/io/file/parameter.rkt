#lang racket/base

(provide current-force-delete-permissions)

(define current-force-delete-permissions
  (make-parameter #t (lambda (v) (and v #t))))

