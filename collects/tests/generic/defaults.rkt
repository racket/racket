#lang racket

(require racket/generic
         (prefix-in s: racket/stream))

(define-generics stream
  (stream-first stream)
  (stream-rest stream)
  (stream-empty? stream)
  #:defaults
  ([list?
    (define my-car car)
    (define stream-first my-car)
    (define stream-rest cdr)
    (define stream-empty? null?)]
   [s:stream?
    (define stream-first s:stream-first)
    (define stream-rest s:stream-rest)
    (define stream-empty? s:stream-empty?)]))

(module+ test
  (require rackunit)

  (define l1 '(1 2))

  (check-true (stream? l1))
  (check-false (stream-empty? l1))
  (check-equal? (stream-first l1) 1)

  (define l2 (stream-rest l1))
  (check-true (stream? l2))
  (check-false (stream-empty? l2))
  (check-equal? (stream-first l2) 2)

  (define l3 (stream-rest l2))
  (check-true (stream? l3))
  (check-true (stream-empty? l3))

  (define l4 (s:stream 1 2 3))
  (check-true (stream? l4))
  (check-false (stream-empty? l4))
  (check-equal? (stream-first l4) 1)
  (check-equal? (stream-first (stream-rest l4)) 2))
