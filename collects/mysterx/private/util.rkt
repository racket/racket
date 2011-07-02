;;; util.rkt -- utility procedures for MysterX
#lang scheme/base

(require scheme/string)
(provide (all-defined-out))

(define (fold-strings-with-spaces strs) (string-join strs " "))

(define (map-to-string f)
  (lambda (lst) (fold-strings-with-spaces (map f lst))))

(define (empty-string? s) (equal? "" s))

(define (bool->string v) (if v "true" "false"))

(define (exact-with-bounds? n lo hi) (and (exact-integer? n) (<= lo n hi)))

(define (list-pos v lst)
  (for/or ([x (in-list lst)] [i (in-naturals)]) (and (eq? x v) i)))

(define (remove-ws cs) ; remove leading whitespace
  (cond [(null? cs) '()]
        [(char-whitespace? (car cs)) (remove-ws (cdr cs))]
        [else cs]))

(define (symbols->string syms) ; '(a b c ...) => "a b c ..."
  (fold-strings-with-spaces (map symbol->string syms)))

(define (hex-digit-string? elt) (regexp-match? #px"(?i:^#[0-9a-f]{6}$)" elt))

(define (hex-color-string? s) (and (string? s) (hex-digit-string? s)))

(define (empty-property-error p)
  (error (format "Empty value for property ~a" p)))
