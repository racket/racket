#lang racket/base

;; Functions that sample from functions, and functions that create memoized samplers.

(require racket/match racket/flonum racket/math racket/contract racket/list
         "contract-doc.rkt"
         "math.rkt"
         "axis-transform.rkt")

(provide (all-defined-out))

(define (build-linear-seq start step num)
  (for/list ([n  (in-range num)])
    (+ start (* n step))))

(defproc (linear-seq [start real?] [end real?] [num exact-nonnegative-integer?]
                     [#:start? start? boolean? #t]
                     [#:end? end? boolean? #t]) (listof real?)
  (cond
    [(zero? num)  empty]
    ; ambiguous request: arbitrarily return start
    [(and start? end? (= 1 num))  (list start)]
    [else
     (define size (- end start))
     (define step (/ size (cond [(and start? end?)  (- num 1)]
                                [(or start? end?)   (- num 1/2)]
                                [else               num])))
     (define real-start
       (cond [start?  start]
             [else    (+ start (* 1/2 step))]))
     
     (build-linear-seq real-start step num)]))

(defproc (linear-seq* [points (listof real?)] [num exact-nonnegative-integer?]
                      [#:start? start? boolean? #t]
                      [#:end? end? boolean? #t]) (listof real?)
  (let/ec return
    (when (empty? points) (raise-type-error 'linear-seq* "nonempty (listof real?)" points))
    
    (define pts (list->vector points))
    (define len (vector-length pts))
    
    (define indexes (linear-seq 0 (sub1 len) num #:start? start? #:end? end?))
    (define int-parts (map floor indexes))
    (define frac-parts (map - indexes int-parts))
    (map (λ (i f)
           (if (= i (sub1 len))
               (vector-ref pts i)
               (blend (vector-ref pts (add1 i)) (vector-ref pts i) f)))
         int-parts frac-parts)))

(struct mapped-function (f fmap) #:transparent
  #:property prop:procedure
  (λ (g x) ((mapped-function-f g) x)))

(define (map* f xs)
  (match f
    #;; gives obviously wrong chaperone error (tries to apply a hash?):
    [(mapped-function _ fmap)  (fmap xs)]
    [(? mapped-function?)  ((mapped-function-fmap f) xs)]
    [_  (map f xs)]))

(defproc (nonlinear-seq [start real?] [end real?] [num exact-nonnegative-integer?]
                        [transform axis-transform/c]
                        [#:start? start? boolean? #t]
                        [#:end? end? boolean? #t]) (listof real?)
  (match-define (invertible-function _ finv) (apply-transform transform start end))
  (map finv (linear-seq start end num #:start? start? #:end? end?)))

(define 2pi (* 2 pi))

(define ((2d-polar->3d-function f) x y z)
  (let ([x  (exact->inexact x)]
        [y  (exact->inexact y)]
        [z  (exact->inexact z)])
    (define-values (θ ρ)
      (cond [(and (fl= x 0.0) (fl= y 0.0))  (values 0.0 0.0)]
            [else  (values (flmodulo (flatan2 y x) 2pi)
                           (flatan (fl/ z (distance x y))))]))
    (define r (exact->inexact (f θ ρ)))
    (fl- r (distance x y z))))

(define (sample-parametric f t-min t-max samples)
  (map* f (linear-seq t-min t-max samples)))

(define (sample-polar f θ-min θ-max samples)
  (define θs (linear-seq θ-min θ-max samples))
  (define rs (map* f θs))
  (map polar->cartesian θs rs))

(define (sample-2d-polar f θ-min θ-max θ-samples ρ-min ρ-max ρ-samples)
  (for*/list ([θ  (in-list (linear-seq θ-min θ-max θ-samples))]
              [ρ  (in-list (linear-seq ρ-min ρ-max ρ-samples))])
    (3d-polar->3d-cartesian θ ρ (f θ ρ))))

(define ((make-function->sampler transform-thnk) f)
  (define memo (make-hash))
  (λ (x-min x-max x-samples)
    (define tx (transform-thnk))
    (hash-ref! memo (vector x-min x-max x-samples tx)
               (λ ()
                 (define xs (nonlinear-seq x-min x-max x-samples tx))
                 (list xs (map* f xs))))))

(define ((make-2d-function->sampler transform-x-thnk transform-y-thnk) f)
  (define memo (make-hash))
  (λ (x-min x-max x-samples y-min y-max y-samples)
    (define tx (transform-x-thnk))
    (define ty (transform-y-thnk))
    (hash-ref! memo (vector x-min x-max x-samples tx y-min y-max y-samples ty)
               (λ ()
                 (define xs (nonlinear-seq x-min x-max x-samples tx))
                 (define ys (nonlinear-seq y-min y-max y-samples ty))
                 (list xs ys (for/vector #:length y-samples ([y  (in-list ys)])
                               (for/vector #:length x-samples ([x  (in-list xs)])
                                 (f x y))))))))

(define ((make-3d-function->sampler transform-x-thnk transform-y-thnk transform-z-thnk) f)
  (define memo (make-hash))
  (λ (x-min x-max x-samples y-min y-max y-samples z-min z-max z-samples)
    (define tx (transform-x-thnk))
    (define ty (transform-y-thnk))
    (define tz (transform-z-thnk))
    (hash-ref! memo (vector x-min x-max x-samples tx
                            y-min y-max y-samples ty
                            z-min z-max z-samples tz)
               (λ ()
                 (define xs (nonlinear-seq x-min x-max x-samples tx))
                 (define ys (nonlinear-seq y-min y-max y-samples ty))
                 (define zs (nonlinear-seq z-min z-max z-samples tz))
                 (list xs ys zs (for/vector #:length z-samples ([z  (in-list zs)])
                                  (for/vector #:length y-samples ([y  (in-list ys)])
                                    (for/vector #:length x-samples ([x  (in-list xs)])
                                      (f x y z)))))))))

(define (2d-sample->list zss)
  (for*/list ([zs  (in-vector zss)]
              [z   (in-vector zs)])
    z))

(define (3d-sample->list dsss)
  (for*/list ([dss  (in-vector dsss)]
              [ds   (in-vector dss)]
              [d    (in-vector ds)])
    d))
