#lang scheme/base

(provide valid-version? version->list version<?)

(define rx:version
  #px"^(0|[1-9][0-9]*)[.](0|(0|[1-9][0-9]*)([.](0|[1-9][0-9]*)){0,2}(?<![.]0))$")

(define (valid-version? s)
  (and (string? s) (regexp-match? rx:version s)))

;; returns a list of 4 integers (see src/mzscheme/src/schvers.h)
(define (version->list str)
  (let ([ver (map string->number (regexp-split #rx"[.]" str))])
    (case (length ver)
      [(2) (append ver '(0 0))]
      [(3) (append ver '(0))]
      [(4) ver]
      [else (error 'version->list "bad version: ~e" str)])))

(define (version<? a b)
  (let loop ([a (version->list a)]
             [b (version->list b)])
    (cond [(null? a) #f]
          [(< (car a) (car b)) #t]
          [(> (car a) (car b)) #f]
          [else (loop (cdr a) (cdr b))])))
