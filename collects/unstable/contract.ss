#lang scheme

(define path-element?
  (or/c path-string? (symbols 'up 'same)))
;; Eli: We already have a notion of "path element" which is different
;;   from this (see `string->path-element') .

(define port-number? (between/c 1 65535))

(define non-empty-string/c
  (and/c string?
         (lambda (s) (not (zero? (string-length s))))))
;; Eli: If this gets in, there should also be versions for bytes, lists, and
;;   vectors.

(provide/contract
 [non-empty-string/c contract?]
 [path-element? contract?]
 [port-number? contract?])
