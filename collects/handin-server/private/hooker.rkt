#lang racket/base

(require "config.rkt" "logger.rkt" "reloadable.rkt")

(provide hook)

(define hook-file #f)
(define hook-proc #f)

(define (hook what alist)
  (let ([file (get-conf 'hook-file)])
    (when file
      (unless (equal? file hook-file)
        (set! hook-file file)
        (set! hook-proc (auto-reload-procedure `(file ,(path->string file))
                                               'hook)))
      (hook-proc what (current-session) alist))))
