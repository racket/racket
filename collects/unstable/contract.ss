#lang scheme

(define path-element?
  (or/c path-string? (symbols 'up 'same)))

(define port-number? (between/c 1 65535))

(define non-empty-string/c
  (and/c string?
         (lambda (s) (not (zero? (string-length s))))))

(provide/contract
 [non-empty-string/c contract?]
 [path-element? contract?]
 [port-number? contract?])