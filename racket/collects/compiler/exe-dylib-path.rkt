#lang racket/base
(require racket/contract/base
         "private/macfw.rkt")

(provide
 (contract-out
  [find-matching-library-path (path-string? string? . -> . (or/c #f string?))]
  [update-matching-library-path (path-string? string? string? . -> . void?)]))

(define (find-matching-library-path exe str)
  (get-current-framework-path exe str))

(define (update-matching-library-path exe str new-str)
  (update-framework-path new-str #:as-given? #t
                         exe
                         #f
                         #:matching (list str)))
