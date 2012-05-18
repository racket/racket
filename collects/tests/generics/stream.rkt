#lang racket

(require racket/generics racket/stream)

(define-struct list-stream (v)
  #:property prop:stream
  (methods generic-stream
    (define (stream-empty? generic-stream)
      (empty? (list-stream-v generic-stream)))
    (define (stream-first generic-stream)
      (first (list-stream-v generic-stream)))
    (define (stream-rest generic-stream)
      (rest (list-stream-v generic-stream)))))


(module+ test
  (require rackunit)

  (define l1 (list-stream '(1 2)))

  (check-true (stream? l1))
  (check-false (stream-empty? l1))
  (check-equal? (stream-first l1) 1)

  (define l2 (stream-rest l1))
  (check-true (stream? l2))
  (check-false (stream-empty? l2))
  (check-equal? (stream-first l2) 2)

  (define l3 (stream-rest l2))
  (check-true (stream? l3))
  (check-true (stream-empty? l3)))
