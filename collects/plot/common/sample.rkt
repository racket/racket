#lang racket/base

;; Functions that sample from functions, and functions that create memoized samplers.

(require racket/match racket/flonum racket/math racket/contract racket/list
         "contract.rkt"
         "contract-doc.rkt"
         "math.rkt"
         "axis-transform.rkt"
         "parameters.rkt"
         "contract.rkt"
         "format.rkt"
         "ticks.rkt")

(provide (all-defined-out))

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

;; ===================================================================================================
;; Common memoized samplers

(define function->sampler (make-function->sampler plot-x-transform))
(define inverse->sampler (make-function->sampler plot-y-transform))
(define 2d-function->sampler (make-2d-function->sampler plot-x-transform plot-y-transform))
(define 3d-function->sampler
  (make-3d-function->sampler plot-x-transform plot-y-transform plot-z-transform))

;; ===================================================================================================
;; Contour ticks

(defproc (contour-ticks [z-min real?] [z-max real?]
                        [levels (or/c 'auto exact-positive-integer? (listof real?))]
                        [intervals? boolean?]) (listof tick?)
  (define epsilon (expt 10 (- (digits-for-range z-min z-max))))
  (match-define (ticks layout format) (plot-z-ticks))
  (define ts
    (cond [(eq? levels 'auto)  (filter pre-tick-major?
                                       (layout z-min z-max (plot-z-max-ticks) (plot-z-transform)))]
          [else  (define zs (cond [(list? levels)  (filter (λ (z) (<= z-min z z-max)) levels)]
                                  [else  (linear-seq z-min z-max levels #:start? #f #:end? #f)]))
                 (map (λ (z) (pre-tick z #t)) zs)]))
  (define all-ts
    (cond [intervals?
           (let* ([ts  (cond [((abs (- z-min (pre-tick-value (first ts)))) . < . epsilon)  ts]
                             [else  (cons (pre-tick z-min #t) ts)])]
                  [ts  (cond [((abs (- z-max (pre-tick-value (last ts)))) . < . epsilon)  ts]
                             [else  (append ts (list (pre-tick z-max #t)))])])
             ts)]
          [else
           (let* ([ts  (cond [((abs (- z-min (pre-tick-value (first ts)))) . >= . epsilon)  ts]
                             [else  (rest ts)])]
                  [ts  (cond [((abs (- z-max (pre-tick-value (last ts)))) . >= . epsilon)  ts]
                             [else  (take ts (- (length ts) 1))])])
             ts)]))
  (match-define (list (pre-tick zs majors) ...) all-ts)
  (define labels (format z-min z-max all-ts))
  (map tick zs majors labels))

(defproc (auto-contour-values [z-min real?] [z-max real?]) (listof real?)
  (define ts (default-z-ticks z-min z-max))
  (let* ([zs  (map pre-tick-value (filter pre-tick-major? ts))]
         [zs  (if (= (first zs) z-min) (rest zs) zs)]
         [zs  (if (= (last zs) z-max) (take zs (sub1 (length zs))) zs)])
    zs))
