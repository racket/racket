#lang racket/base

;; Functions that sample from functions, and functions that create memoized samplers.

(require racket/match racket/flonum racket/math racket/contract racket/list
         "contract-doc.rkt"
         "math.rkt"
         "axis-transform.rkt")

(provide (all-defined-out))

(defproc (build-linear-seq [start real?] [step real?] [num exact-nonnegative-integer?]) (listof real?)
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

(defproc (nonlinear-seq [start real?] [end real?] [num exact-nonnegative-integer?]
                        [transform axis-transform/c]
                        [#:start? start? boolean? #t]
                        [#:end? end? boolean? #t]) (listof real?)
  (match-define (invertible-function _ finv) (apply-axis-transform transform start end))
  (map finv (linear-seq start end num #:start? start? #:end? end?)))

(struct mapped-function (f fmap) #:transparent
  #:property prop:procedure
  (λ (g x) ((mapped-function-f g) x)))

(define (map* f xs)
  (match f
    #;; gives obviously wrong chaperone error (tries to apply a hash?):
    [(mapped-function _ fmap)  (fmap xs)]
    [(? mapped-function?)  ((mapped-function-fmap f) xs)]
    [_  (map f xs)]))

;; ===================================================================================================
;; Making memoized samplers

(struct sample (xs ys y-min y-max) #:transparent)
(struct 2d-sample (xs ys zss z-min z-max) #:transparent)
(struct 3d-sample (xs ys zs dsss d-min d-max) #:transparent)

(defcontract sample/c (list/c (listof real?) (listof real?)))
(defcontract sampler/c (real? real? exact-nonnegative-integer? . -> . sample?))

(defcontract 2d-sampler/c (real? real? exact-nonnegative-integer?
                                 real? real? exact-nonnegative-integer?
                                 . -> . 2d-sample?))

(defcontract 3d-sampler/c (real? real? exact-nonnegative-integer?
                                 real? real? exact-nonnegative-integer?
                                 real? real? exact-nonnegative-integer?
                                 . -> . 3d-sample?))

(defproc (make-function->sampler [transform-thnk (-> axis-transform/c)]
                                 ) ((real? . -> . real?) . -> . sampler/c)
  (λ (f)
    (define memo (make-hash))
    (λ (x-min x-max x-samples)
      (define tx (transform-thnk))
      (hash-ref! memo (vector x-min x-max x-samples tx)
                 (λ ()
                   (define xs (nonlinear-seq x-min x-max x-samples tx))
                   (define ys (map* f xs))
                   (define rys (filter regular? ys))
                   (define-values (y-min y-max)
                     (cond [(empty? rys)  (values #f #f)]
                           [else  (values (apply min* rys) (apply max* rys))]))
                   (sample xs ys y-min y-max))))))

(defproc (make-2d-function->sampler [transform-x-thnk (-> axis-transform/c)]
                                    [transform-y-thnk (-> axis-transform/c)]
                                    ) ((real? real? . -> . real?) . -> . 2d-sampler/c)
  (λ (f)
    (define memo (make-hash))
    (λ (x-min x-max x-samples y-min y-max y-samples)
      (define tx (transform-x-thnk))
      (define ty (transform-y-thnk))
      (hash-ref! memo (vector x-min x-max x-samples tx y-min y-max y-samples ty)
                 (λ ()
                   (define xs (nonlinear-seq x-min x-max x-samples tx))
                   (define ys (nonlinear-seq y-min y-max y-samples ty))
                   (define z-min #f)
                   (define z-max #f)
                   (define zss (for/vector #:length y-samples ([y  (in-list ys)])
                                 (for/vector #:length x-samples ([x  (in-list xs)])
                                   (let ([z  (f x y)])
                                     (when (regular? z)
                                       (unless (and z-min (z . >= . z-min)) (set! z-min z))
                                       (unless (and z-max (z . <= . z-max)) (set! z-max z)))
                                     z))))
                   (2d-sample xs ys zss z-min z-max))))))

(defproc (make-3d-function->sampler [transform-x-thnk (-> axis-transform/c)]
                                    [transform-y-thnk (-> axis-transform/c)]
                                    [transform-z-thnk (-> axis-transform/c)]
                                    ) ((real? real? real? . -> . real?) . -> . 3d-sampler/c)
  (λ (f)
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
                   (define d-min #f)
                   (define d-max #f)
                   (define dsss (for/vector #:length z-samples ([z  (in-list zs)])
                                  (for/vector #:length y-samples ([y  (in-list ys)])
                                    (for/vector #:length x-samples ([x  (in-list xs)])
                                      (let ([d  (f x y z)])
                                        (when (regular? d)
                                          (unless (and d-min (d . >= . d-min)) (set! d-min d))
                                          (unless (and d-max (d . <= . d-max)) (set! d-max d)))
                                        d)))))
                   (3d-sample xs ys zs dsss d-min d-max))))))
