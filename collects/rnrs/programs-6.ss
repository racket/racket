#lang scheme/base

(provide command-line exit)

(define (command-line)
  (cons (path->string (find-system-path 'run-file))
        (vector->list (current-command-line-arguments))))
