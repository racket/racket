#lang racket

(provide in-row)

(require 
 math/array
 math/matrix
 (for-syntax math/matrix)
 (for-template math/matrix))

(define-sequence-syntax in-row
  (λ () #'in-row/proc)
  (λ (stx)
    (syntax-case stx ()
      [[(x) (_ M-expr r-expr)]
       #'((x)
          (:do-in
           ([(M r n d) 
             (let ([M1 M-expr])
               (define-values (rd cd) (matrix-dimensions M1))
               (values M1 r-expr rd cd))])
           (begin 
             (unless (matrix? M) 
               (raise-type-error 'in-row "expected matrix, got ~a" M))
             (unless (integer? r) 
               (raise-type-error 'in-row "expected row number, got ~a" r)))
           ([j 0])
           (< j n)
           ([(x) (matrix-ref d (+ (* r n) j))])
           #true
           #true
           [(+ j 1)]))]
      [[(i x) (_ M-expr r-expr)]
       #'((i x)
          (:do-in
           ([(M r n d) 
             (let ([M1 M-expr])
               (define-values (rd cd) (matrix-dimensions M1))
               (values M1 r-expr rd cd))])
           (begin 
             (unless (matrix? M) 
               (raise-type-error 'in-row "expected matrix, got ~a" M))
             (unless (integer? r) 
               (raise-type-error 'in-row "expected row number, got ~a" r)))
           ([j 0])
           (< j n)
           ([(x) (matrix-ref d (+ (* r n) j))]
            [(i) j])
           #true
           #true
           [(+ j 1)]))])))
