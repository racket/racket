#lang racket/base

;; Functions that sample from functions, and functions that create memoized samplers.

(require racket/match racket/flonum racket/math racket/contract racket/list
         "math.rkt"
         "axis-transform.rkt"
         "parameters.rkt"
         "contract.rkt")

(provide (all-defined-out))

(struct mapped-function (f fmap) #:transparent
  #:property prop:procedure
  (λ (g x) ((mapped-function-f g) x)))

(struct mapped-function/bounds mapped-function (x-min x-max) #:transparent)

(define (make-mapped-function fmap)
  (mapped-function (λ (x) (first (fmap (list x)))) fmap))

(define (make-mapped-function/bounds fmap x-min x-max)
  (mapped-function/bounds (λ (x) (first (fmap (list x)))) fmap x-min x-max))

(define (map* f xs)
  (match f
    #;; gives obviously wrong chaperone error (tries to apply a hash?):
    [(mapped-function _ fmap)  (fmap xs)]
    [(? mapped-function?)  ((mapped-function-fmap f) xs)]
    [_  (map f xs)]))

(define (nonlinear-seq x-min x-max samples transform #:start? [start? #t] #:end? [end? #t])
  (match-define (invertible-function _ finv) (transform x-min x-max))
  (map finv (linear-seq x-min x-max samples #:start? start? #:end? end?)))

(define ((2d-polar->3d-function f) x y z)
  (let ([x  (exact->inexact x)]
        [y  (exact->inexact y)]
        [z  (exact->inexact z)])
    (define-values (θ ρ)
      (cond [(and (fl= x 0.0) (fl= y 0.0))  (values 0.0 0.0)]
            [else  (values (flmodulo (atan y x) 2pi)
                           (atan (fl/ z (fldist2 x y))))]))
    (define r (exact->inexact (f θ ρ)))
    (fl- r (fldist3 x y z))))

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
