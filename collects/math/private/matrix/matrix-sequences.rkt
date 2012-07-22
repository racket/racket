#lang racket

(provide for/matrix for*/matrix)

(require 
 math/array
 math/matrix
 (for-syntax math/matrix)
 (for-template math/matrix))


;;; COMPREHENSIONS

(define (flat-vector->vector-of-vectors m n fv)
  (for/vector #:length m ([i (in-range m)])
    (for/vector #:length n ([j (in-range n)])
      (vector-ref fv (+ (* i n) j)))))

(define (flat-vector->matrix m n fv)
  (vector->matrix
   (flat-vector->vector-of-vectors m n fv)))

; (for/matrix m n (clause ...) . defs+exprs)
;    Return an  m x n  matrix with elements from the last expr.
;    The first n values produced becomes the first row.
;    The next n values becomes the second row and so on.
;    The bindings in clauses run in parallel.
(define-syntax (for/matrix stx)
  (syntax-case stx ()
    [(_ m-expr n-expr (clause ...) . defs+exprs)
     (syntax/loc stx
       (let ([m m-expr] [n n-expr])
         (define flat-vector           
           (for/vector #:length (* m n)
             (clause ...) . defs+exprs))
         ; TODO (efficiency): Use a flat-vector->array instead
         (flat-vector->matrix m n flat-vector)))]))

; (for*/matrix m n (clause ...) . defs+exprs)
;    Return an  m x n  matrix with elements from the last expr.
;    The first n values produced becomes the first row.
;    The next n values becomes the second row and so on.
;    The bindings in clauses run nested.
; (for*/matrix m n #:column (clause ...) . defs+exprs)
;    Return an  m x n  matrix with elements from the last expr.
;    The first m values produced becomes the first column.
;    The next m values becomes the second column and so on.
;    The bindings in clauses run nested.
(define-syntax (for*/matrix stx)
  (syntax-case stx ()
    [(_ m-expr n-expr #:column (clause ...) . defs+exprs)
     (syntax/loc stx
       (let* ([m m-expr] 
              [n n-expr]
              [v (make-vector (* m n) 0)]
              [w (for*/vector #:length (* m n) (clause ...) . defs+exprs)])
         (for* ([i (in-range m)] [j (in-range n)])
           (vector-set! v (+ (* i n) j)
                        (vector-ref w (+ (* j m) i))))
         (flat-vector->matrix m n v)))]
    [(_ m-expr n-expr (clause ...) . defs+exprs)
     (syntax/loc stx
       (let ([m m-expr] [n n-expr])
         (flat-vector->matrix 
          m n (for*/vector #:length (* m n) (clause ...) . defs+exprs))))]))

(define-syntax (for*/matrix: stx)
  (syntax-case stx ()
    [(_ m-expr n-expr #:column (clause ...) . defs+exprs)
     (syntax/loc stx
       (let* ([m m-expr] 
              [n n-expr]
              [v (make-vector (* m n) 0)]
              [w (for*/vector #:length (* m n) (clause ...) . defs+exprs)])
         (for* ([i (in-range m)] [j (in-range n)])
           (vector-set! v (+ (* i n) j)
                        (vector-ref w (+ (* j m) i))))
         (flat-vector->matrix m n v)))]
    [(_ m-expr n-expr (clause ...) . defs+exprs)
     (syntax/loc stx
       (let ([m m-expr] [n n-expr])
         (flat-vector->matrix 
          m n (for*/vector #:length (* m n) (clause ...) . defs+exprs))))]))


#;(define-sequence-syntax in-row
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
