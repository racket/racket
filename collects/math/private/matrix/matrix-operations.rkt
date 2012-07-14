#lang typed/racket

(require math/array)
(require "matrix-types.rkt"
         "matrix-constructors.rkt")

(provide matrix-scale-row
         matrix-swap-rows
         matrix-add-scaled-row
         matrix-gauss-eliminate
         matrix-ref)

(define-syntax (inline-matrix-scale-row stx)
  (syntax-case stx ()
    [(_ i c)
     (syntax/loc stx
       (λ (arr)
         (let ([arr  (array-lazy arr)])
           (define ds (unsafe-array-shape arr))
           (define g (unsafe-array-proc arr))
           (cond
             [(< i 0)
              (error 'matrix-scale-row "row index must be non-negative, got ~a" i)]
             [(not (< i (vector-ref ds 0)))
              (error 'matrix-scale-row "row index must be smaller than the number of rows, got ~a" i)]
             [else
              (unsafe-lazy-array ds (λ: ([js : (Vectorof Index)]) 
                                      (if (= i (vector-ref js 0))
                                          (* c (g js))
                                          (g js))))]))))]))

(: matrix-scale-row : (Matrix Number) Integer Number -> (Result-Matrix Number))
(define (matrix-scale-row a i c)
  ((inline-matrix-scale-row i c) a))

(define-syntax (inline-matrix-swap-rows stx)
  (syntax-case stx ()
    [(_ i j)
     (syntax/loc stx
       (λ (arr)
         (let ([arr  (array-lazy arr)])
           (define ds (unsafe-array-shape arr))
           (define g (unsafe-array-proc arr))
           (cond
             [(< i 0)
              (error 'matrix-swap-rows "row index must be non-negative, got ~a" i)]
             [(< j 0)
              (error 'matrix-swap-rows "row index must be non-negative, got ~a" j)]
             [(not (< i (vector-ref ds 0)))
              (error 'matrix-swap-rows "row index must be smaller than the number of rows, got ~a" i)]
             [(not (< j (vector-ref ds 0)))
              (error 'matrix-swap-rows "row index must be smaller than the number of rows, got ~a" j)]
             [else
              (unsafe-lazy-array ds (λ: ([js : (Vectorof Index)]) 
                                      (cond
                                        [(= i (vector-ref js 0)) 
                                         (g (vector j (vector-ref js 1)))]
                                        [(= j (vector-ref js 0)) 
                                         (g (vector i (vector-ref js 1)))]
                                        [else                    
                                         (g js)])))]))))]))

(: matrix-swap-rows : (Matrix Number) Integer Integer -> (Result-Matrix Number))
(define (matrix-swap-rows a i j)
  ((inline-matrix-swap-rows i j) a))

(define-syntax (inline-matrix-add-scaled-row stx)
  (syntax-case stx ()
    [(_ i c j)
     (syntax/loc stx
       (λ (arr)
         (let ([arr  (array-lazy arr)])
           (define ds (unsafe-array-shape arr))
           (define g (unsafe-array-proc arr))
           (cond
             [(< i 0)
              (error 'matrix-add-scaled-row "row index must be non-negative, got ~a" i)]
             [(< j 0)
              (error 'matrix-add-scaled-row "row index must be non-negative, got ~a" j)]
             [(not (< i (vector-ref ds 0)))
              (error 'matrix-add-scaled-row 
                     "row index must be smaller than the number of rows, got ~a" i)]
             [(not (< j (vector-ref ds 0)))
              (error 'matrix-add-scaled-row 
                     "row index must be smaller than the number of rows, got ~a" j)]
             [else
              (unsafe-lazy-array ds (λ: ([js : (Vectorof Index)]) 
                                      (if (= i (vector-ref js 0))
                                          (+ (g js) (* c (g (vector j (vector-ref js 1)))))
                                          (g js))))]))))]))

(: matrix-add-scaled-row : (Matrix Number) Integer Number Integer -> (Result-Matrix Number))
(define (matrix-add-scaled-row a i c j)
  ((inline-matrix-add-scaled-row i c j) a))

(: flmatrix-add-scaled-row : (Matrix Flonum) Index Flonum Index -> (Result-Matrix Flonum))
(define (flmatrix-add-scaled-row a i c j)
  ((inline-matrix-add-scaled-row i c j) a))


;;; GAUSS ELIMINATION / ROW ECHELON FORM

(: matrix-ref : (Matrix Number) Integer Integer -> Number)
(define (matrix-ref M i j)
  ((inst array-ref Number) M (list i j)))

(: matrix-gauss-eliminate : 
   (case-> ((Matrix Number) Boolean Boolean -> (Result-Matrix Number))
           ((Matrix Number) Boolean         -> (Result-Matrix Number))
           ((Matrix Number)                 -> (Result-Matrix Number))))
(define (matrix-gauss-eliminate M [unitize-pivot-row? #f] [partial-pivoting? #t])
  (define dims (matrix-dimensions M))
  (define m (vector-ref dims 0))
  (define n (vector-ref dims 1))
  (: loop : (Integer Integer (Matrix Number) Integer (Listof Integer) -> (Matrix Number)))
  (define (loop i j ; i from 0 to m
                M
                k   ; count rows without pivot
                without-pivot) 
    (cond
      [(or (= i m) (= j n)) M]
      [else
       ; find row to become pivot
       (define p
         (if partial-pivoting?
             ; find element with maximal absolute value
             (let: max-loop : (U Boolean Integer) 
               ([l : Integer i] ; i<=l<m
                [max-current : Real -inf.0]
                [max-index : Integer i])
               (cond
                 [(= l m) max-index]
                 [else 
                  (let ([v (magnitude (matrix-ref M l j))])
                    (if (> (magnitude (matrix-ref M l j)) max-current)
                        (max-loop (+ l 1) v l)
                        (max-loop (+ l 1) max-current max-index)))]))
             ; find non-zero element in column
             (let: first-loop : (U Boolean Integer) 
               ([l : Integer i]) ; i<=l<m
               (cond
                 [(= l m) #f]
                 [(not (zero? (matrix-ref M l j))) l]
                 [else (first-loop (+ l 1))]))))
       (cond
         [(boolean? p) ; p=#f
          ; no pivot found
          (loop i (+ j 1) M (+ k 1) (cons j without-pivot))]
         [(integer? p)
          ; swap if neccessary
          (let* ([M (matrix-swap-rows M i p)]
                 ; now we now (i,j) is a pivot
                 [M ; maybe scale row
                  (if unitize-pivot-row?
                      (let ([pivot (matrix-ref M i j)])
                        (if (zero? pivot)
                            M
                            (matrix-scale-row M i (/ pivot))))
                      M)])
            (let ([pivot (matrix-ref M i j)])
              ; remove elements below pivot
              (let l-loop ([l (+ i 1)] [M M])
                (if (= l m)
                    (loop (+ i 1) (+ j 1) M k without-pivot)
                    (let ([x_lj (matrix-ref M l j)])
                      (l-loop (+ l 1)
                              (if (zero? x_lj)
                                  M
                                  (matrix-add-scaled-row M l (- (/ x_lj pivot)) i))))))))])]))
  (array-lazy (loop 0 0 M 0 '())))
