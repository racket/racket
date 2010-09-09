#lang racket

(define-struct/contract s1 ([x any/c] [y any/c]))
(define-struct s2 (x y))
(define-values (make-s3 s3-x)
  (let ()
    (define-struct s3 (x y))
    (values (contract (-> any/c any/c s3?)
                      make-s3
                      'pos
                      'neg)
            (contract (-> s3? any/c) s3-x 'pos 'neg))))

(define s4-func
  (let ([ns (make-base-namespace)])
    (parameterize ([current-namespace ns]) 
      (eval '(module m1 racket/base
               (require racket/contract)
               (define-struct s4 (x y))
               (provide/contract
                [make-s4 (-> any/c any/c s4?)]
                [s4-x (-> s4? any/c)])))
      (eval '(module m2 racket/base
               (require 'm1)
               (define (s4-func x) (s4-x (make-s4 x x)))
               (provide s4-func)))
      (eval '(require 'm2))
      (eval 's4-func))))

(define (t f)
 (time
  (let loop ([n 10000])
    (unless (zero? n)
      (f 1) (f 1) (f 1) (f 1) (f 1)
      (f 1) (f 1) (f 1) (f 1) (f 1)
      (f 1) (f 1) (f 1) (f 1) (f 1)
      (f 1) (f 1) (f 1) (f 1) (f 1)
      (f 1) (f 1) (f 1) (f 1) (f 1)
      (loop (- n 1))))))

(t (λ (x) (s1-x (make-s1 x x))))
(t (λ (x) (s2-x (make-s2 x x))))
(t (λ (x) (s3-x (make-s3 x x))))
(t s4-func)
