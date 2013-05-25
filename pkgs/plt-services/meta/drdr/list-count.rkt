#lang racket

(define list/count
  (or/c exact-nonnegative-integer? (listof bytes?)))
(define lc->number
  (match-lambda
    [(? number? x)
     x]
    [(? list? x)
     (length x)]))
(define lc->list
  (match-lambda
    [(? number? x)
     empty]
    [(? list? x)
     x]))
(define lc-zero?
  (match-lambda
    [(? number? x)
     (zero? x)]
    [(? list? x)
     (eq? empty x)]))
(define (lc+ x y)
  (cond
    [(number? x)
     (+ x (lc->number y))]
    [(number? y)
     (+ (lc->number x) y)]
    [else
     (append x y)]))

(provide/contract
 [list/count contract?]
 [lc+ (list/count list/count . -> . list/count)]
 [lc->number (list/count . -> . exact-nonnegative-integer?)]
 [lc->list (list/count . -> . (listof bytes?))]
 [lc-zero? (list/count . -> . boolean?)])
