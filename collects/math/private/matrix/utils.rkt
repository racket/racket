#lang typed/racket/base

(require racket/match
         racket/string
         math/array
         "matrix-types.rkt")

(provide (all-defined-out))

(: format-matrices/error ((Listof (Array Any)) -> String))
(define (format-matrices/error as)
  (string-join (map (λ: ([a : (Array Any)]) (format "~e" a)) as)))

(: matrix-shapes (Symbol (Matrix Any) (Matrix Any) * -> (Values Index Index)))
(define (matrix-shapes name arr . brrs)
  (define-values (m n) (matrix-shape arr))
  (unless (andmap (λ: ([brr : (Matrix Any)])
                    (match-define (vector bm bn) (array-shape brr))
                    (and (= bm m) (= bn n)))
                  brrs)
    (error name
           "matrices must have the same shape; given ~a"
           (format-matrices/error (cons arr brrs))))
  (values m n))

(: matrix-multiply-shape ((Matrix Any) (Matrix Any) -> (Values Index Index Index)))
(define (matrix-multiply-shape arr brr)
  (define-values (ad0 ad1) (matrix-shape arr))
  (define-values (bd0 bd1) (matrix-shape brr))
  (unless (= ad1 bd0)
    (error 'matrix-multiply
           "1st argument column size and 2nd argument row size are not equal; given ~e and ~e"
           arr brr))
  (values ad0 ad1 bd1))

(: ensure-matrix (All (A) Symbol (Array A) -> (Array A)))
(define (ensure-matrix name a)
  (if (matrix? a) a (raise-argument-error name "matrix?" a)))
