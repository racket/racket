#lang typed/racket
#:optimize

(provide Stream stream-cons stream-car stream-cdr empty-stream?)

(define-type Stream
  (All (A)
    (Rec S
      (U Null (Boxof (U (-> (Pair A S))
                        (Pair A S)))))))

(: empty-stream? : (All (A) ((Stream A) -> Boolean)))
(define (empty-stream? stream) (null? stream))

(define-syntax-rule (stream-cons x stream)
  (box (lambda () (cons x stream))))

(: stream-car : (All (A) ((Stream A) -> A)))
(define (stream-car stream)
  (if (null? stream)
    (error 'stream-car "empty stream: ~e" stream)
    (let ([p (unbox stream)])
      (if (procedure? p)
        (let ([pair (p)])
          (set-box! stream pair)
          (car pair))
        (car p)))))

(: stream-cdr : (All (A) ((Stream A) -> (Stream A))))
(define (stream-cdr stream)
  (if (null? stream)
    (error 'stream-cdr "empty stream: ~e" stream)
    (let ([p (unbox stream)])
      (if (procedure? p)
        (let ([pair (p)])
          (set-box! stream pair)
          (cdr pair))
        (cdr p)))))

(: stream : (All (A) (A * -> (Stream A))))
(define (stream . xs)
  (: loop : (All (A) ((Listof A) -> (Stream A))))
  (define (loop xs)
    (if (null? xs)
      '()
      (box (cons (car xs) (loop (cdr xs))))))
  (loop xs))

(: stream->list : (All (A) ((Stream A) -> (Listof A))))
(define (stream->list stream)
  (if (null? stream)
    '()
    (cons (stream-car stream) (stream->list (stream-cdr stream)))))

(: rotate : (All (A) ((Stream A) (Listof A) (Stream A) -> (Stream A))))
(define (rotate frnt rer accum)
  (let ([carrer (car rer)])
    ;; Manually expanded `stream-cons', and added type annotations
    (if (empty-stream? frnt)
        (stream-cons carrer accum)
        (stream-cons
         (stream-car frnt)
         ((inst rotate A)
          (stream-cdr frnt)
          (cdr rer)
          (box (lambda () (cons carrer accum))))))))
