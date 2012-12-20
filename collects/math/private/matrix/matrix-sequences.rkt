#lang racket

(provide in-row
         in-column)

(require math/array
         "matrix-types.rkt"
         "matrix-basic.rkt"
         "matrix-constructors.rkt"
         )

(define (in-row/proc M r)
  (define-values (m n) (matrix-shape M))
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
               (define-values (rd cd) (matrix-shape M1))
               (values M1 r-expr rd 
                       (mutable-array-data
                        (array->mutable-array M1))))])
           (begin 
             (unless (matrix? M) 
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
               (define-values (rd cd) (matrix-shape M1))
               (values M1 r-expr rd 
                       (mutable-array-data
                        (array->mutable-array M1))))])
           (begin 
             (unless (matrix? M) 
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
  (define-values (m n) (matrix-shape M))
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
               (define-values (rd cd) (matrix-shape M1))
               (values M1 s-expr rd cd 
                       (mutable-array-data
                        (array->mutable-array M1))))])
           (begin 
             (unless (matrix? M) 
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
               (define-values (rd cd) (matrix-shape M1))
               (values M1 s-expr rd cd
                       (mutable-array-data
                        (array->mutable-array M1))))])
           (begin 
             (unless (matrix? M) 
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

(define-syntax (for/matrix-sum: stx)
  (syntax-case stx ()
    [(_ : type (for:-clause ...) . defs+exprs)
     (syntax/loc stx
       (let ()
         (define: sum : (U False (Matrix Number)) #f)
         (for: (for:-clause ...)
           (define a (let () . defs+exprs))
           (set! sum (if sum (array+ (assert sum) a) a)))
         (assert sum)))]))
#|
;;;
;;; SEQUENCES
;;;

(: in-row/proc : (Matrix Number) Integer -> (Sequenceof Number))
(define (in-row/proc M r)
  (define-values (m n) (matrix-shape M))
  (make-do-sequence
   (λ ()
     (values
      ; pos->element
      (λ: ([j : Index]) (matrix-ref M r j))
      ; next-pos
      (λ: ([j : Index]) (assert (+ j 1) index?))
      ; initial-pos
      0
      ; continue-with-pos?
      (λ: ([j : Index ]) (< j n))
      #f #f))))

(: in-column/proc : (Matrix Number) Integer -> (Sequenceof Number))
(define (in-column/proc M s)
  (define-values (m n) (matrix-shape M))
  (make-do-sequence
   (λ ()
     (values
      ; pos->element
      (λ: ([i : Index]) (matrix-ref M i s))
      ; next-pos
      (λ: ([i : Index]) (assert (+ i 1) index?))
      ; initial-pos
      0
      ; continue-with-pos?
      (λ: ([i : Index]) (< i m))
      #f #f))))

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
               (define-values (rd cd) (matrix-shape M1))
               (values M1 r-expr rd 
                       (mutable-array-data
                        (array->mutable-array M1))))])
           (begin 
             (unless (matrix? M) 
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
               (define-values (rd cd) (matrix-shape M1))
               (values M1 r-expr rd 
                       (mutable-array-data
                        (array->mutable-array M1))))])
           (begin 
             (unless (matrix? M) 
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

(define-sequence-syntax in-column
  (λ () #'in-column/proc)
  (λ (stx)
    (syntax-case stx ()
      ; M-expr evaluates to column
      [[(x) (_ M-expr)]
       #'((x)
          (:do-in
           ([(M n m d) 
             (let ([M1 M-expr])
               (define-values (rd cd) (matrix-shape M1))
               (values M1 rd cd 
                       (mutable-array-data
                        (array->mutable-array M1))))])
           (unless (matrix? M) 
             (raise-type-error 'in-row "expected matrix, got ~a" M))
           ([j 0])
           (< j n)
           ([(x) (vector-ref d j)])
           #true
           #true
           [(+ j 1)]))]
      ; M-expr evaluats to matrix, s-expr to the column index
      [[(x) (_ M-expr s-expr)]
       #'((x)
          (:do-in
           ([(M s n m d) 
             (let ([M1 M-expr])
               (define-values (rd cd) (matrix-shape M1))
               (values M1 s-expr rd cd 
                       (mutable-array-data
                        (array->mutable-array M1))))])
           (begin 
             (unless (matrix? M) 
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
               (define-values (rd cd) (matrix-shape M1))
               (values M1 s-expr rd cd
                       (mutable-array-data
                        (array->mutable-array M1))))])
           (begin 
             (unless (matrix? M) 
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
|#
#;
(module* test #f
  (require rackunit)
  ; "matrix-sequences.rkt"
  (check-equal? (for/list ([x     (in-row (vector->matrix 2 2 #(1 2 3 4)) 1)]) x)
                '(3 4))
  (check-equal? (for/list ([(i x) (in-row (vector->matrix 2 2 #(1 2 3 4)) 1)])
                  (list i x))
                '((0 3) (1 4)))
  (check-equal? (for/list ([x     (in-column (vector->matrix 2 2 #(1 2 3 4)) 1)]) x)
                '(2 4))
  (check-equal? (for/list ([(i x) (in-column (vector->matrix 2 2 #(1 2 3 4)) 1)])
                  (list i x))
                '((0 2) (1 4))))
