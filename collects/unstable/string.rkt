#lang racket/base
(require racket/contract/base)

;; added by stamourv

(define (regexp-filter r log)
  (for/list ([l (in-list log)] #:when (regexp-match r l))
    l))
(provide/contract
 [regexp-filter ((or/c string? bytes? regexp? byte-regexp?)
                 (listof (or/c string? bytes? path? input-port?))
                 . -> . (listof (or/c string? bytes? path? input-port?)))])
