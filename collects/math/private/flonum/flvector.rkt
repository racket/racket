#lang typed/racket/base

(require racket/fixnum
         "../unsafe.rkt"
         "flonum-functions.rkt"
         "flvector-syntax.rkt")

(provide
 (all-from-out "flvector-syntax.rkt")
 ;; Construction
 unsafe-flvector-copy!
 flvector-copy!
 ;; Conversion
 list->flvector
 flvector->list
 vector->flvector
 flvector->vector
 ;; Pointwise operations
 flvector-scale
 flvector-sqr
 flvector-sqrt
 flvector-abs
 flvector+
 flvector*
 flvector-
 flvector/
 flvector-min
 flvector-max
 ;; Sum
 flvector-sum
 flvector-sums
 flsum)

;; ===================================================================================================
;; flvector-copy!

(: unsafe-flvector-copy! (FlVector Integer FlVector Integer Integer -> Void))
(define (unsafe-flvector-copy! dest dest-start src src-start src-end)
  (let loop ([i dest-start] [j src-start])
    (when (j . unsafe-fx< . src-end)
      (unsafe-flvector-set! dest i (unsafe-flvector-ref src j))
      (loop (unsafe-fx+ i 1) (unsafe-fx+ j 1)))))

(: flvector-copy! (case-> (FlVector Integer FlVector -> Void)
                          (FlVector Integer FlVector Integer -> Void)
                          (FlVector Integer FlVector Integer Integer -> Void)))
(define flvector-copy!
  (case-lambda
    [(dest dest-start src)
     (flvector-copy! dest dest-start src 0 (flvector-length src))]
    [(dest dest-start src src-start)
     (flvector-copy! dest dest-start src src-start (flvector-length src))]
    [(dest dest-start src src-start src-end)
     (define dest-len (flvector-length dest))
     (define src-len (flvector-length src))
     (cond [(or (dest-start . < . 0) (dest-start . > . dest-len))
            (raise-argument-error 'flvector-copy! (format "Index <= ~e" dest-len) 1
                                  dest dest-start src src-start src-end)]
           [(or (src-start . < . 0) (src-start . > . src-len))
            (raise-argument-error 'flvector-copy! (format "Index <= ~e" src-len) 3
                                  dest dest-start src src-start src-end)]
           [(or (src-end . < . 0) (src-end . > . src-len))
            (raise-argument-error 'flvector-copy! (format "Index <= ~e" src-len) 4
                                  dest dest-start src src-start src-end)]
           [(src-end . < . src-start)
            (error 'flvector-copy! "ending index is smaller than starting index")]
           [((- dest-len dest-start) . < . (- src-end src-start))
            (error 'flvector-copy! "not enough room in target vector")]
           [else
            (unsafe-flvector-copy! dest dest-start src src-start src-end)])]))

;; ===================================================================================================
;; Conversion

(: list->flvector ((Listof Real) -> FlVector))
(define (list->flvector vs)
  (for/flvector: #:length (length vs) ([v  (in-list vs)])
    (fl v)))

(: flvector->list (FlVector -> (Listof Float)))
(define (flvector->list xs)
  (for/list: : (Listof Float) ([x  (in-flvector xs)]) x))

(: vector->flvector ((Vectorof Real) -> FlVector))
(define (vector->flvector vs)
  (for/flvector: #:length (vector-length vs) ([v  (in-vector vs)])
    (fl v)))

(: flvector->vector (FlVector -> (Vectorof Float)))
(define (flvector->vector xs)
  (for/vector: #:length (flvector-length xs) ([x  (in-flvector xs)]) : Flonum
    x))

;; ===================================================================================================
;; Pointwise operations

(define-syntax-rule (lift1 f)
  (λ: ([arr : FlVector])
    (inline-flvector-map f arr)))

(define-syntax-rule (lift2 f)
  (λ: ([arr0 : FlVector] [arr1 : FlVector])
    (inline-flvector-map f arr0 arr1)))

(: flvector-scale (FlVector Float -> FlVector))
(define (flvector-scale arr y) (inline-flvector-map (λ: ([x : Flonum]) (fl* x y)) arr))

(: flvector-sqr (FlVector -> FlVector))
(define flvector-sqr (lift1 (λ: ([x : Flonum]) (fl* x x))))

(: flvector-sqrt (FlVector -> FlVector))
(define flvector-sqrt (lift1 flsqrt))

(: flvector-abs (FlVector -> FlVector))
(define flvector-abs (lift1 flabs))

(: flvector+ (FlVector FlVector -> FlVector))
(define flvector+ (lift2 fl+))

(: flvector* (FlVector FlVector -> FlVector))
(define flvector* (lift2 fl*))

(: flvector- (case-> (FlVector -> FlVector)
                     (FlVector FlVector -> FlVector)))
(define flvector-
  (case-lambda:
    [([arr0 : FlVector])
     (inline-flvector-map (λ: ([x : Float]) (fl- 0.0 x)) arr0)]
    [([arr0 : FlVector] [arr1 : FlVector])
     (inline-flvector-map fl- arr0 arr1)]))

(: flvector/ (case-> (FlVector -> FlVector)
                     (FlVector FlVector -> FlVector)))
(define flvector/
  (case-lambda:
    [([arr0 : FlVector])
     (inline-flvector-map (λ: ([x : Float]) (fl/ 1.0 x)) arr0)]
    [([arr0 : FlVector] [arr1 : FlVector])
     (inline-flvector-map fl/ arr0 arr1)]))

(: flvector-min  (FlVector FlVector -> FlVector))
(define flvector-min (lift2 flmin))

(: flvector-max  (FlVector FlVector -> FlVector))
(define flvector-max (lift2 flmax))

;; ===================================================================================================
;; Summation

#|
Algorithm adapted from:

J R Shewchuk. Adaptive Precision Floating-Point Arithmetic and Fast Geometric Predicates.
Discrete & Computational Geometry, 1996, vol 18, pp 305--363.
|#

(: flvector-sum (FlVector -> Flonum))
;; Returns the sum of the elements in xs in a way that incurs rounding error only once
(define (flvector-sum xs)
  (define n (flvector-length xs))
  ;; Vector of remainders
  (define rs (make-flvector n))
  ;; Loop over `xs'
  (let i-loop ([#{i : Nonnegative-Fixnum} 0]
               ;; p = Number of valid remainders in `rs'
               [#{p : Nonnegative-Fixnum} 0])
    (cond
      [(i . fx< . n)
       ;; Add x consecutively to each remainder, storing the remainder of *those* additions in `rs'
       (let j-loop ([#{j : Nonnegative-Fixnum} 0]
                    ;; q = Number of remainders generated by this j-loop:
                    [#{q : Nonnegative-Fixnum} 0]
                    [x  (unsafe-flvector-ref xs i)])
         (cond
           [(j . fx< . p)
            (define r (unsafe-flvector-ref rs j))
            ;; Get the largest of x and r, or x if it's not rational
            (let-values ([(x r)  (if ((flabs x) . fl< . (flabs r)) (values r x) (values x r))])
              ;; Add with remainder
              (define z (fl+ x r))
              (define-values (hi lo)
                (cond [(flrational? z)  (values z (fl- r (fl- z x)))]
                      [else  (values x r)]))
              (cond [(fl= lo 0.0)
                     ;; No remainder: don't store (makes average case O(n*log(n)))
                     (j-loop (unsafe-fx+ j 1) q hi)]
                    [else
                     ;; Store the remainder, increment the counter
                     (unsafe-flvector-set! rs q lo)
                     (j-loop (unsafe-fx+ j 1) (unsafe-fx+ q 1) hi)]))]
           [else
            ;; Store the sum so far as the last remainder
            (unsafe-flvector-set! rs q x)
            (i-loop (fx+ i 1) (unsafe-fx+ q 1))]))]
      [else
       ;; Add all the remainders
       (let j-loop ([#{j : Nonnegative-Fixnum} 0] [acc  0.0])
         (cond [(j . fx< . p)  (j-loop (unsafe-fx+ j 1) (fl+ acc (unsafe-flvector-ref rs j)))]
               [else  acc]))])))

(: flvector-sums (FlVector -> FlVector))
;; Returns the partial sums of the elements in xs in a way that incurs rounding error only once
;; for each
;; This function works just like `flvector-sum', but keeps track of partial sums instead of
;; summing all the remainders at the end
(define (flvector-sums xs)
  (define n (flvector-length xs))
  (define rs (make-flvector n))
  (define ss (make-flvector n))
  (let i-loop ([#{i : Nonnegative-Fixnum} 0]
               [#{p : Nonnegative-Fixnum} 0])
    (cond
      [(i . fx< . n)
       (let j-loop ([#{j : Nonnegative-Fixnum} 0]
                    [#{q : Nonnegative-Fixnum} 0]
                    [x (unsafe-flvector-ref xs i)]
                    [s 0.0])
         (cond
           [(j . fx< . p)
            (define r (unsafe-flvector-ref rs j))
            (let-values ([(x r)  (if ((flabs x) . fl< . (flabs r)) (values r x) (values x r))])
              (define z (fl+ x r))
              (define-values (hi lo)
                (cond [(flrational? z)  (values z (fl- r (fl- z x)))]
                      [else  (values x r)]))
              (cond [(fl= lo 0.0)
                     (j-loop (unsafe-fx+ j 1) q hi s)]
                    [else
                     (unsafe-flvector-set! rs q lo)
                     (j-loop (unsafe-fx+ j 1) (unsafe-fx+ q 1) hi (fl+ s lo))]))]
           [else
            (unsafe-flvector-set! rs q x)
            (unsafe-flvector-set! ss i (fl+ s x))
            (i-loop (fx+ i 1) (unsafe-fx+ q 1))]))]
      [else  ss])))

(: flsum ((Listof Flonum) -> Flonum))
(define (flsum xs) (flvector-sum (list->flvector xs)))
