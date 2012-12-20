#lang racket

(require math/array
         typed/racket/base
         "matrix-types.rkt"
         "matrix-constructors.rkt")

(provide for/matrix
         for*/matrix
         for/matrix:
         for*/matrix:)

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
         (vector->matrix m n flat-vector)))]))

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
         (vector->matrix m n v)))]
    [(_ m-expr n-expr (clause ...) . defs+exprs)
     (syntax/loc stx
       (let ([m m-expr] [n n-expr])
         (vector->matrix 
          m n (for*/vector #:length (* m n) (clause ...) . defs+exprs))))]))


(define-syntax (for/column: stx)
  (syntax-case stx ()
    [(_ : type m-expr (for:-clause ...) . defs+exprs)
     (syntax/loc stx
       (let ()
         (define: m : Index m-expr)
         (define: flat-vector : (Vectorof Number) (make-vector m 0))
         (for: ([i (in-range m)] for:-clause ...)
           (define x (let () . defs+exprs))
           (vector-set! flat-vector i x))
         (vector->col-matrix flat-vector)))]))

(define-syntax (for/matrix: stx)
  (syntax-case stx ()
    [(_ : type m-expr n-expr #:column (for:-clause ...) . defs+exprs)
     (syntax/loc stx
       (let ()
         (define: m : Index m-expr)
         (define: n : Index n-expr)
         (define: m*n : Index (assert (* m n) index?))
         (define: v : (Vectorof Number) (make-vector m*n 0))
         (define: k : Index 0)
         (for: ([i (in-range m*n)] for:-clause ...)
           (define x (let () . defs+exprs))
           (vector-set! v (+ (* n (remainder k m)) (quotient k m)) x)
           (set! k (assert (+ k 1) index?)))         
         (vector->matrix m n v)))]
    [(_ : type m-expr n-expr (for:-clause ...) . defs+exprs)
     (syntax/loc stx
       (let ()
         (define: m : Index m-expr)
         (define: n : Index n-expr)
         (define: m*n : Index (assert (* m n) index?))
         (define: v : (Vectorof Number) (make-vector m*n 0))
         (for: ([i (in-range m*n)] for:-clause ...)
           (define x (let () . defs+exprs))
           (vector-set! v i x))
         (vector->matrix m n v)))]))

(define-syntax (for*/matrix: stx)
  (syntax-case stx ()
    [(_ : type m-expr n-expr #:column (for:-clause ...) . defs+exprs)
     (syntax/loc stx
       (let ()
         (define: m : Index m-expr)
         (define: n : Index n-expr)
         (define: m*n : Index (assert (* m n) index?))
         (define: v : (Vectorof Number) (make-vector m*n 0))
         (define: k : Index 0)
         (for*: (for:-clause ...)
           (define x (let () . defs+exprs))
           (vector-set! v (+ (* n (remainder k m)) (quotient k m)) x)
           (set! k (assert (+ k 1) index?)))
         (vector->matrix m n v)))]
    [(_ : type m-expr n-expr (for:-clause ...) . defs+exprs)
     (syntax/loc stx
       (let ()
         (define: m : Index m-expr)
         (define: n : Index n-expr)
         (define: m*n : Index (assert (* m n) index?))
         (define: v : (Vectorof Number) (make-vector m*n 0))
         (define: i : Index 0) 
         (for*: (for:-clause ...)
           (define x (let () . defs+exprs))
           (vector-set! v i x)
           (set! i (assert (+ i 1) index?)))
         (vector->matrix m n v)))]))
#;
(module* test #f
  (require rackunit)
  ; "matrix-sequences.rkt"
  ; These work in racket not in typed racket
  (check-equal? (matrix->list* (for*/matrix 2 3 ([i 2] [j 3]) (+ i j)))
                '[[0 1 2] [1 2 3]])
  (check-equal? (matrix->list* (for*/matrix 2 3 #:column ([i 2] [j 3]) (+ i j)))
                '[[0 2 2] [1 1 3]])
  (check-equal? (matrix->list* (for*/matrix 2 2 #:column ([i 4]) i)) 
                '[[0 2] [1 3]])
  (check-equal? (matrix->list* (for/matrix 2 2 ([i 4]) i)) 
                '[[0 1] [2 3]])
  (check-equal? (matrix->list* (for/matrix 2 3 ([i 6] [j (in-range 6 12)]) (+ i j)))
                '[[6 8 10] [12 14 16]]))
