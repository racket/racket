#lang typed/racket/base

(require racket/fixnum
         racket/string
         (for-syntax racket/base syntax/parse)
         "flonum-functions.rkt"
         "../unsafe.rkt"
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
 flvector-round
 flvector-floor
 flvector-ceiling
 flvector-truncate
 flvector-abs
 flvector-sqr
 flvector-sqrt
 flvector-log
 flvector-exp
 flvector-sin
 flvector-cos
 flvector-tan
 flvector-asin
 flvector-acos
 flvector-atan
 flvector+
 flvector*
 flvector-
 flvector/
 flvector-expt
 flvector-min
 flvector-max
 flvector=
 flvector<
 flvector<=
 flvector>
 flvector>=
 ;;
 flvector-sums)

;; ===================================================================================================
;; flvector-copy

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
  (define n (length vs))
  (define xs (make-flvector n))
  (let loop ([#{i : Nonnegative-Fixnum} 0] [vs vs])
    (cond [(i . < . n)  (unsafe-flvector-set! xs i (real->double-flonum (unsafe-car vs)))
                        (loop (+ i 1) (unsafe-cdr vs))]
          [else  xs])))

(: flvector->list (FlVector -> (Listof Float)))
(define (flvector->list xs)
  (for/list: : (Listof Float) ([x  (in-flvector xs)]) x))

(: vector->flvector ((Vectorof Real) -> FlVector))
(define (vector->flvector vs)
  (define n (vector-length vs))
  (define xs (make-flvector n))
  (let loop ([#{i : Nonnegative-Fixnum} 0])
    (cond [(i . < . n)  (unsafe-flvector-set! xs i (real->double-flonum (unsafe-vector-ref vs i)))
                        (loop (+ i 1))]
          [else  xs])))

(: flvector->vector (FlVector -> (Vectorof Float)))
(define (flvector->vector xs)
  (define n (flvector-length xs))
  (define vs (make-vector n 0.0))
  (let loop ([#{i : Nonnegative-Fixnum} 0])
    (cond [(i . < . n)  (unsafe-vector-set! vs i (unsafe-flvector-ref xs i))
                        (loop (+ i 1))]
          [else  vs])))

;; ===================================================================================================
;; Pointwise operations

(define-syntax (lift1 stx)
  (syntax-case stx ()
    [(_ f)  (syntax/loc stx (λ (arr) (flvector-map f arr)))]))

(define-syntax (lift2 stx)
  (syntax-case stx ()
    [(_ f)  (syntax/loc stx (λ (arr1 arr2) (flvector-map f arr1 arr2)))]))

(define-syntax-rule (lift-comparison name comp)
  (λ (xs1 xs2)
    (define n1 (flvector-length xs1))
    (define n2 (flvector-length xs2))
    (unless (= n1 n2) (error name "flvectors must be the same length; given lengths ~e and ~e" n1 n2))
    (build-vector
     n1 (λ: ([j : Index])
          (comp (unsafe-flvector-ref xs1 j)
                (unsafe-flvector-ref xs2 j))))))

(: flvector-scale (FlVector Float -> FlVector))
(define (flvector-scale arr y) (flvector-map (λ (x) (fl* x y)) arr))

(: flvector-round    (FlVector -> FlVector))
(: flvector-floor    (FlVector -> FlVector))
(: flvector-ceiling  (FlVector -> FlVector))
(: flvector-truncate (FlVector -> FlVector))
(: flvector-abs  (FlVector -> FlVector))
(: flvector-sqr  (FlVector -> FlVector))
(: flvector-sqrt (FlVector -> FlVector))
(: flvector-log  (FlVector -> FlVector))
(: flvector-exp  (FlVector -> FlVector))
(: flvector-sin  (FlVector -> FlVector))
(: flvector-cos  (FlVector -> FlVector))
(: flvector-tan  (FlVector -> FlVector))
(: flvector-asin (FlVector -> FlVector))
(: flvector-acos (FlVector -> FlVector))
(: flvector-atan (FlVector -> FlVector))

(: flvector+ (FlVector FlVector -> FlVector))
(: flvector* (FlVector FlVector -> FlVector))
(: flvector- (case-> (FlVector -> FlVector)
                    (FlVector FlVector -> FlVector)))
(: flvector/ (case-> (FlVector -> FlVector)
                    (FlVector FlVector -> FlVector)))
(: flvector-expt (FlVector FlVector -> FlVector))
(: flvector-min  (FlVector FlVector -> FlVector))
(: flvector-max  (FlVector FlVector -> FlVector))

(: flvector=  (FlVector FlVector -> (Vectorof Boolean)))
(: flvector<  (FlVector FlVector -> (Vectorof Boolean)))
(: flvector<= (FlVector FlVector -> (Vectorof Boolean)))
(: flvector>  (FlVector FlVector -> (Vectorof Boolean)))
(: flvector>= (FlVector FlVector -> (Vectorof Boolean)))

(define flvector-round    (lift1 flround))
(define flvector-floor    (lift1 flfloor))
(define flvector-ceiling  (lift1 flceiling))
(define flvector-truncate (lift1 fltruncate))
(define flvector-abs  (lift1 flabs))
(define flvector-sqr  (lift1 (λ: ([x : Float]) (fl* x x))))
(define flvector-sqrt (lift1 flsqrt))
(define flvector-log  (lift1 fllog))
(define flvector-exp  (lift1 flexp))
(define flvector-sin  (lift1 flsin))
(define flvector-cos  (lift1 flcos))
(define flvector-tan  (lift1 fltan))
(define flvector-asin (lift1 flasin))
(define flvector-acos (lift1 flacos))
(define flvector-atan (lift1 flatan))

(define flvector+ (lift2 fl+))
(define flvector* (lift2 fl*))

(define flvector-
  (case-lambda
    [(arr)  (flvector-map (λ: ([x : Float]) (fl- 0.0 x)) arr)]
    [(arr1 arr2)  (flvector-map fl- arr1 arr2)]))

(define flvector/
  (case-lambda
    [(arr)  (flvector-map (λ: ([x : Float]) (fl/ 1.0 x)) arr)]
    [(arr1 arr2)  (flvector-map fl/ arr1 arr2)]))

(define flvector-expt (lift2 flexpt))
(define flvector-min  (lift2 flmin))
(define flvector-max  (lift2 flmax))

(define flvector=  (lift-comparison 'flvector=  fl=))
(define flvector<  (lift-comparison 'flvector<  fl<))
(define flvector<= (lift-comparison 'flvector<= fl<=))
(define flvector>  (lift-comparison 'flvector>  fl>))
(define flvector>= (lift-comparison 'flvector>= fl>=))

;; ===================================================================================================

(: flvector-sums (FlVector -> FlVector))
(define (flvector-sums xs)
  (define n (flvector-length xs))
  (define rs (make-flvector n))
  (define ss (make-flvector n))
  (let j-loop ([#{j : Nonnegative-Fixnum} 0]
               [#{m : Nonnegative-Fixnum} 0])
    (cond
      [(j . fx< . n)
       (define x (unsafe-flvector-ref xs j))
       (let p-loop ([#{p : Nonnegative-Fixnum} 0]
                    [#{x : Flonum} x]
                    [#{s : Flonum} 0.0]
                    [#{i : Nonnegative-Fixnum} 0])
         (cond
           [(p . fx< . m)
            (define r (unsafe-flvector-ref rs p))
            (let-values ([(x r)  (if ((flabs x) . fl< . (flabs r)) (values r x) (values x r))])
              (define z (fl+ x r))
              (define-values (hi lo)
                (cond [(flrational? z)  (values z (fl- r (fl- z x)))]
                      [else  (values x r)]))
              (cond [(fl= lo 0.0)
                     (p-loop (unsafe-fx+ p 1) hi s i)]
                    [else
                     (unsafe-flvector-set! rs i lo)
                     (p-loop (unsafe-fx+ p 1) hi (fl+ s lo) (unsafe-fx+ i 1))]))]
           [else
            (unsafe-flvector-set! rs i x)
            (unsafe-flvector-set! ss j (fl+ s x))
            (j-loop (fx+ j 1) (unsafe-fx+ i 1))]))]
      [else  ss])))
