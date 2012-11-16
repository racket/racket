#lang racket
(provide for/matrix
         for*/matrix
         in-row
         in-column)

(require math/array
         (except-in math/matrix in-row in-column))

;;; COMPREHENSIONS

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

; TODO: The following is uncommented until matrix+ can be imported.

; (for/matrix-sum (clause ...) . defs+exprs)
;    Return the matrix sum of all matrices produced by the last expr.
;    The bindings in clauses are parallel.

;(define-syntax (for/matrix-sum stx)
;  (syntax-case stx ()
;    [(_ (clause ...) . defs+exprs)
;     (syntax/loc stx
;       (let ([ms (for/list (clause ...) . defs+exprs)])
;         (foldl matrix+ (first ms) (rest ms))))]))
;
;(equal? (let ([M (flat-vector->matrix 2 2  #(1 2 3 4))]) 
;          (for/matrix-sum ([i 3]) M))
;        (flat-vector->matrix 2 2  #(3 6 9 12)))
;(equal? (let ([M (flat-vector->matrix 2 2  #(1 2 3 4))]) 
;          (for/matrix-sum ([i 2] [j 2]) M))
;        (flat-vector->matrix 2 2  #(2 4 6 8)))

; (for*/matrix-sum (clause ...) . defs+exprs)
;    Return the matrix sum of all matrices produced by the last expr.
;    The bindings in clauses are in nested.

;(define-syntax (for*/matrix-sum stx)
;  (syntax-case stx ()
;    [(_ (clause ...) . defs+exprs)
;     (syntax/loc stx
;       (let ([ms (for*/list (clause ...) . defs+exprs)])
;         (foldl matrix+ (first ms) (rest ms))))]))
;
;(equal? (let ([M (flat-vector->matrix 2 2 #(1 2 3 4))])
;          (for*/matrix-sum ([i 2] [j 2]) M))
;        (flat-vector->matrix 2 2 #(4 8 12 16)))


;;;
;;; SEQUENCES
;;;

(define (in-row/proc M r)
  (define-values (m n) (matrix-dimensions M))
  (make-do-sequence
   (λ ()
     (values
      ; pos->element
      (λ (j) (matrix-ref M r j))
      ; next-pos
      (λ (j) (+ j 1))
      ; initial-pos
      0
      ; continue-with-pos?
      (λ (j) (< j n))
      #f #f ))))

; (in-row M i]
;     Returns a sequence of all elements of row i,
;     that is xi0, xi1, xi2, ...
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
               (values M1 r-expr rd 
                       (mutable-array-data
                        (array->mutable-array M1))))])
           (begin 
             (unless (array-matrix? M) 
               (raise-type-error 'in-row "expected matrix, got ~a" M))
             (unless (integer? r) 
               (raise-type-error 'in-row "expected row number, got ~a" r))
             (unless (and (integer? r) (and (<= 0 r ) (< r n))) 
               (raise-type-error 'in-row "expected row number, got ~a" r)))
           ([j 0])
           (< j n)
           ([(x) (vector-ref d (+ (* r n) j))])
           #true
           #true
           [(+ j 1)]))]
      [[(i x) (_ M-expr r-expr)]
       #'((i x)
          (:do-in
           ([(M r n d) 
             (let ([M1 M-expr])
               (define-values (rd cd) (matrix-dimensions M1))
               (values M1 r-expr rd 
                       (mutable-array-data
                        (array->mutable-array M1))))])
           (begin 
             (unless (array-matrix? M) 
               (raise-type-error 'in-row "expected matrix, got ~a" M))
             (unless (integer? r) 
               (raise-type-error 'in-row "expected row number, got ~a" r)))
           ([j 0])
           (< j n)
           ([(x) (vector-ref d (+ (* r n) j))]
            [(i) j])
           #true
           #true
           [(+ j 1)]))]
      [[_ clause] (raise-syntax-error 
                   'in-row "expected (in-row <matrix> <row>)" #'clause #'clause)])))

; (in-column M j]
;     Returns a sequence of all elements of column j,
;     that is x0j, x1j, x2j, ...


(define (in-column/proc M s)
  (define-values (m n) (matrix-dimensions M))
  (make-do-sequence
   (λ ()
     (values
      ; pos->element
      (λ (i) (matrix-ref M i s))
      ; next-pos
      (λ (i) (+ i 1))
      ; initial-pos
      0
      ; continue-with-pos?
      (λ (i) (< i m))
      #f #f ))))

(define-sequence-syntax in-column
  (λ () #'in-column/proc)
  (λ (stx)
    (syntax-case stx ()
      [[(x) (_ M-expr s-expr)]
       #'((x)
          (:do-in
           ([(M s n m d) 
             (let ([M1 M-expr])
               (define-values (rd cd) (matrix-dimensions M1))
               (values M1 s-expr rd cd 
                       (mutable-array-data
                        (array->mutable-array M1))))])
           (begin 
             (unless (array-matrix? M) 
               (raise-type-error 'in-row "expected matrix, got ~a" M))
             (unless (integer? s) 
               (raise-type-error 'in-row "expected col number, got ~a" s))
             (unless (and (integer? s) (and (<= 0 s ) (< s m))) 
               (raise-type-error 'in-col "expected col number, got ~a" s)))
           ([j 0])
           (< j m)
           ([(x) (vector-ref d (+ (* j n) s))])
           #true
           #true
           [(+ j 1)]))]
      [[(i x) (_ M-expr s-expr)]
       #'((x)
          (:do-in
           ([(M s n m d) 
             (let ([M1 M-expr])
               (define-values (rd cd) (matrix-dimensions M1))
               (values M1 s-expr rd cd
                       (mutable-array-data
                        (array->mutable-array M1))))])
           (begin 
             (unless (array-matrix? M) 
               (raise-type-error 'in-column "expected matrix, got ~a" M))
             (unless (integer? s) 
               (raise-type-error 'in-column "expected col number, got ~a" s))
             (unless (and (integer? s) (and (<= 0 s ) (< s m))) 
               (raise-type-error 'in-column "expected col number, got ~a" s)))
           ([j 0])
           (< j m)
           ([(x) (vector-ref d (+ (* j n) s))]
            [(i) j])
           #true
           #true
           [(+ j 1)]))]
      [[_ clause] (raise-syntax-error 
                   'in-column "expected (in-column <matrix> <column>)" #'clause #'clause)])))

(module* test #f
  (require (except-in math/matrix in-row in-column)
           rackunit)
  ; "matrix-sequences.rkt"
  ; These work in racket not in typed racket
  (check-equal? (matrix->list (for*/matrix 2 3 ([i 2] [j 3]) (+ i j)))
                '[[0 1 2] [1 2 3]])
  (check-equal? (matrix->list (for*/matrix 2 3 #:column ([i 2] [j 3]) (+ i j)))
                '[[0 2 2] [1 1 3]])
  (check-equal? (matrix->list (for*/matrix 2 2 #:column ([i 4]) i)) 
                '[[0 2] [1 3]])
  (check-equal? (matrix->list (for/matrix 2 2 ([i 4]) i)) 
                '[[0 1] [2 3]])
  (check-equal? (matrix->list (for/matrix 2 3 ([i 6] [j (in-range 6 12)]) (+ i j)))
                '[[6 8 10] [12 14 16]])
  (check-equal? (for/list ([x     (in-row (flat-vector->matrix 2 2 #(1 2 3 4)) 1)]) x)
                '(3 4))
  (check-equal? (for/list ([(i x) (in-row (flat-vector->matrix 2 2 #(1 2 3 4)) 1)])
                  (list i x))
                '((0 3) (1 4)))
  (check-equal? (for/list ([x     (in-column (flat-vector->matrix 2 2 #(1 2 3 4)) 1)]) x)
                '(2 4))
  (check-equal? (for/list ([(i x) (in-column (flat-vector->matrix 2 2 #(1 2 3 4)) 1)])
                  (list i x))
                '((0 2) (1 4))))
