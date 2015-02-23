#lang racket

(require racket/generic racket/stream)

(define-struct list-stream (v)
  #:methods gen:stream
  [(define (stream-empty? generic-stream)
     (empty? (list-stream-v generic-stream)))
   (define (stream-first generic-stream)
     (first (list-stream-v generic-stream)))
   (define (stream-rest generic-stream)
     (rest (list-stream-v generic-stream)))])

(struct vector-stream (i v)
        #:methods gen:stream
        [(define (stream-first x) (vector-ref (vector-stream-v x)
                                              (vector-stream-i x)))
         (define (stream-rest x) (vector-stream (add1 (vector-stream-i x))
                                                (vector-stream-v x)))
         (define (stream-empty? x) (>= (vector-stream-i x)
                                       (vector-length
                                        (vector-stream-v x))))])



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
  (check-true (stream-empty? l3))


  (define s2 (vector-stream 0 '#(1 2 3)))
  (check-equal? (sequence-fold + 0 s2) 6)
  )
