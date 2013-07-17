#lang racket

(require racket/generic
         (prefix-in s: racket/stream))

(define-generics stream
  (stream-first stream)
  (stream-rest stream)
  (stream-empty? stream)
  #:fast-defaults
  ([list?
    (define my-car car)
    (define stream-first my-car)
    (define stream-rest cdr)
    (define stream-empty? null?)])
  #:defaults
  ([s:stream?
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

(struct a ())

(define-generics bool-able
  (to-bool bool-able)
  #:defaults
  ([a? (define (to-bool a) #t)]))

(struct b a ()
  #:methods gen:bool-able
  [(define (to-bool b) #f)])

(module+ test
  (define my-a (a))
  (define my-b (b))

  (check-true (bool-able? my-a))
  (check-true (bool-able? my-b))

  (check-true (to-bool my-a))
  (check-false (to-bool my-b)))

(define-generics nested-stream
  (nested-stream-first  nested-stream)
  (nested-stream-rest   nested-stream)
  (nested-stream-empty? nested-stream)
  #:defaults
  ;; list of streams, yield elements of substreams
  ([(lambda (los) (and (list? los) (andmap nested-stream? los)))
    (define/generic super-first  nested-stream-first)
    (define/generic super-rest   nested-stream-rest)
    (define/generic super-empty? nested-stream-empty?)
    (define (nested-stream-first los)
      (when (stream-empty? los)
        (error 'empty!))
      (if (super-empty? (first los))
          (super-first (rest los))
          (super-first (first los))))
    (define (nested-stream-rest los)
      (when (stream-empty? los)
        (error 'empty!))
      (if (super-empty? (first los))
          (super-rest (rest los))
          (cons (super-rest (first los)) (rest los))))
    (define (nested-stream-empty? los)
      (or (empty? los)
          (and (super-empty? (first los))
               (super-empty? (rest los)))))]
   ;; base case, flat list
   [list?
    (define nested-stream-first  first)
    (define nested-stream-rest   rest)
    (define nested-stream-empty? empty?)]))

(module+ test
  (define (nested-stream->list ns)
    (if (nested-stream-empty? ns)
        '()
        (cons (nested-stream-first ns)
              (nested-stream->list (nested-stream-rest ns)))))

  (define ns1 '())
  (define ns2 '(() ()))
  (define ns3 '((1 2 3) (4 5 6)))
  (define ns4 '((1 2 3) (4 5 6) ()))
  (define ns5 '((1 (2 (3)) (4 (5 (6))))))
  (define ns6 '(() (4 (5 (6)))))

  (check-equal? (nested-stream->list ns1) '())
  (check-equal? (nested-stream->list ns2) '())
  (check-equal? (nested-stream->list ns3) '(1 2 3 4 5 6))
  (check-equal? (nested-stream->list ns4) '(1 2 3 4 5 6))
  (check-equal? (nested-stream->list ns5) '(1 2 3 4 5 6))
  (check-equal? (nested-stream->list ns6) '(4 5 6)))
