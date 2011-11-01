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
  (match-define (invertible-function _ finv) (apply-transform transform start end))
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

(defcontract sample/c (list/c (listof real?) (listof real?)))
(defcontract sampler/c (real? real? exact-nonnegative-integer? . -> . sample/c))

(defcontract 2d-sample/c (list/c (listof real?) (listof real?)
                                 (vectorof (vectorof real?))))
(defcontract 2d-sampler/c (real? real? exact-nonnegative-integer?
                                 real? real? exact-nonnegative-integer?
                                 . -> . 2d-sample/c))

(defcontract 3d-sample/c (list/c (listof real?) (listof real?) (listof real?)
                                 (vectorof (vectorof (vectorof real?)))))
(defcontract 3d-sampler/c (real? real? exact-nonnegative-integer?
                                 real? real? exact-nonnegative-integer?
                                 real? real? exact-nonnegative-integer?
                                 . -> . 3d-sample/c))

(defproc (make-function->sampler [transform-thnk (-> axis-transform/c)]
                                 ) ((real? . -> . real?) . -> . sampler/c)
  (λ (f)
    (define memo (make-hash))
    (λ (x-min x-max x-samples)
      (define tx (transform-thnk))
      (hash-ref! memo (vector x-min x-max x-samples tx)
                 (λ ()
                   (define xs (nonlinear-seq x-min x-max x-samples tx))
                   (list xs (map* f xs)))))))

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
                   (list xs ys (for/vector #:length y-samples ([y  (in-list ys)])
                                 (for/vector #:length x-samples ([x  (in-list xs)])
                                   (f x y)))))))))

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
                   (list xs ys zs (for/vector #:length z-samples ([z  (in-list zs)])
                                    (for/vector #:length y-samples ([y  (in-list ys)])
                                      (for/vector #:length x-samples ([x  (in-list xs)])
                                        (f x y z))))))))))

(defproc (2d-sample->list [zss (vectorof (vectorof real?))]) (listof real?)
  (for*/list ([zs  (in-vector zss)]
              [z   (in-vector zs)])
    z))

(defproc (3d-sample->list [dsss (vectorof (vectorof (vectorof real?)))]) (listof real?)
  (for*/list ([dss  (in-vector dsss)]
              [ds   (in-vector dss)]
              [d    (in-vector ds)])
    d))
