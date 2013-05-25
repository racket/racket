#lang racket

(require (for-syntax racket/match)
         rackunit
         math/matrix)

;; ===================================================================================================
;; Contract tests

(begin-for-syntax
  (define exceptions (list 'matrix 'col-matrix 'row-matrix
                           'matrix-determinant/row-reduction))
  
  (define (looks-like-value? sym)
    (define str (symbol->string sym))
    (and (not (char-upper-case? (string-ref str 0)))
         (not (regexp-match #rx"for/" str))
         (not (regexp-match #rx"for\\*/" str))
         (not (member sym exceptions))))
  
  (define matrix-exports
    (let ()
      (match-define (list (list #f _ ...)
                          (list 1 _ ...)
                          (list 0 matrix-exports ...))
        (syntax-local-module-exports #'math/matrix))
      (filter looks-like-value? matrix-exports)))
  )

(define-syntax (all-exports stx)
  (with-syntax ([(matrix-exports ...)  matrix-exports])
    (syntax/loc stx
      (begin (void matrix-exports) ...))))

(all-exports)

;; ===================================================================================================
;; Comprehensions

(check-equal?
 (for/matrix 2 2 ([i  (in-range 4)]) i)
 (matrix [[0 1] [2 3]]))

(check-equal?
 (for*/matrix 2 2 ([i  (in-range 2)] [j  (in-range 2)]) (+ i j))
 (matrix [[0 1] [1 2]]))

(check-exn exn:fail:contract? (λ () (for/matrix 2 0 () 0)))
(check-exn exn:fail:contract? (λ () (for/matrix 0 2 () 0)))
(check-exn exn:fail:contract? (λ () (for*/matrix 2 0 () 0)))
(check-exn exn:fail:contract? (λ () (for*/matrix 0 2 () 0)))

;; ===================================================================================================
;; Arithmetic and mapping macros

(check-equal? (matrix* (identity-matrix 4) (identity-matrix 4))
              (identity-matrix 4))

(check-equal? (matrix+ (identity-matrix 4) (identity-matrix 4))
              (matrix-scale (identity-matrix 4) 2))

(check-equal? (matrix- (identity-matrix 4) (identity-matrix 4))
              (make-matrix 4 4 0))

(check-equal? (matrix-map (λ (x) (* x -1)) (identity-matrix 4))
              (matrix-scale (identity-matrix 4) -1))
