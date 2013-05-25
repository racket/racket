#lang scheme/base

(require scheme/mpair)

(provide command-line exit)

(define (command-line)
  (mcons (path->string (find-system-path 'run-file))
         (list->mlist (vector->list (current-command-line-arguments)))))
