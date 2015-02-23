#lang racket/base

(provide explode-relpath-string
         module-path-v-string?)

(define (explode-relpath-string p)
  (map (lambda (p)
         (cond [(assoc p '((#"." . same) (#".." . up))) => cdr]
               [else (bytes->path-element p)]))
       (regexp-split #rx#"/+" (string->bytes/utf-8 p))))

(define (module-path-v-string? v)
  (and (regexp-match? #rx"^[-a-zA-Z0-9./]+$" v)
       (not (regexp-match? #rx"^/" v))
       (not (regexp-match? #rx"/$" v))))
