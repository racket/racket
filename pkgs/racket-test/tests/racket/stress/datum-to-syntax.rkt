#lang racket/base

(define l (datum->syntax
           #f
           (for/list ([i 10000])
             'x)))


(let ([now (current-inexact-milliseconds)])
  (let loop ([l l])
    (define v (syntax-e l))
    (cond
      [(null? v) 'done]
      [else
       (loop (datum->syntax #f (cdr v)))]))
  (define t (- (current-inexact-milliseconds) now))
  (unless (t . < . 1000.0)
    (error 'datum->syntax-stress "took too long: ~a" t)))

