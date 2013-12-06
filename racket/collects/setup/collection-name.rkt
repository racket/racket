#lang racket/base

(provide collection-name?
         collection-name-element?)

(define (collection-name? name)
  (and (string? name)
       (andmap collection-name-element? (regexp-split #rx"/" name))))

(define (collection-name-element? name)
  (and (string? name)
       (regexp-match #rx"^[a-zA-Z0-9+_%-]+$" name)
       ;; Using `module-path?' checks that "%" is used apprrpriately:
       (module-path? name)))
