#lang racket/base

;; Functions that sample from functions, and functions that create memoized samplers.

(require racket/match racket/flonum racket/math racket/contract racket/list racket/vector
         unstable/latent-contract/defthing
         "math.rkt"
         "axis-transform.rkt")

(provide (all-defined-out))

(defproc (build-linear-seq [start real?] [step real?]
                           [num exact-nonnegative-integer?]
                           [min-val real? start]
                           [max-val real? (+ start (* (- num 1) step))]
                           ) (listof real?)
  (define n-start (max 0 (inexact->exact (floor (/ (- min-val start) step)))))
  (define n-end (min num (+ (inexact->exact (ceiling (/ (- max-val start) step))) 1)))
  (for*/list ([n  (in-range n-start n-end)]
              [x  (in-value (+ start (* n step)))]
              #:when (<= min-val x max-val))
    x))

(defproc (linear-seq [start real?] [end real?] [num exact-nonnegative-integer?]
                     [#:start? start? boolean? #t]
                     [#:end? end? boolean? #t]) (listof real?)
  (cond
    [(zero? num)  empty]
    ; ambiguous request: arbitrarily return start
    [(and start? end? (= 1 num))  (list start)]
    [(end . < . start)  (reverse (linear-seq end start num #:start? end? #:end? start?))]
    [(end . = . start)  (build-list num (λ _ start))]
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

;; ===================================================================================================

(define (ensure-endpoints xs i-min i-max)
  (cond [(empty? xs)  (cond [(i-min . = . i-max)  (list i-min)]
                            [else  (list i-min i-max)])]
        [else
         (define xs-min (first xs))
         (define xs-max (last xs))
         (let* ([xs  (if (xs-min . <= . i-min) xs (cons i-min xs))]
                [xs  (if (xs-max . >= . i-max) xs (append xs (list i-max)))])
           xs)]))

(defproc (sample-points [outer-ivl rational-ivl?] [inner-ivl ivl?]
                        [num exact-nonnegative-integer?]
                        [transform axis-transform/c id-transform]) (listof real?)
  (let* ([inner-ivl  (ivl-meet inner-ivl outer-ivl)]
         [inner-ivl  (ivl-inexact->exact inner-ivl)]
         [outer-ivl  (ivl-inexact->exact outer-ivl)])
    (match-define (ivl o-min o-max) outer-ivl)
    (match-define (ivl i-min i-max) inner-ivl)
    (match-define (invertible-function f finv) (apply-axis-transform transform o-min o-max))
    (cond
      [(ivl-empty? inner-ivl)  empty]
      [(= num 0)  empty]
      [(or (= o-min o-max) (= num 1))
       (cond [(<= i-min o-min i-max)  (build-list num (λ _ o-min))]
             [else  empty])]
      [else
       (define step (/ (- o-max o-min) (- num 1)))
       (let* ([xs  (map finv (build-linear-seq o-min step num (f i-min) (f i-max)))]
              [xs  (remove-duplicates (map (λ (x) (clamp-real x inner-ivl)) xs))])
         (ensure-endpoints xs i-min i-max))])))

;; ===================================================================================================

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

(defcontract sampler/c
  (-> rational-ivl? exact-nonnegative-integer? sample?))

(defcontract 2d-sampler/c
  (-> (vector/c rational-ivl? rational-ivl?)
      (vector/c exact-nonnegative-integer? exact-nonnegative-integer?)
      2d-sample?))

(defcontract 3d-sampler/c
  (-> (vector/c rational-ivl? rational-ivl? rational-ivl?)
      (vector/c exact-nonnegative-integer? exact-nonnegative-integer? exact-nonnegative-integer?)
      3d-sample?))

(defproc (make-function->sampler [transform-thnk (-> axis-transform/c)]
                                 ) (-> (real? . -> . real?) ivl? sampler/c)
  (λ (g inner-ivl)
    (define f (λ (x) (with-handlers ([exn:fail?  (λ (_) +nan.0)]) (g x))))
    (define memo (make-hash))
    (λ (outer-ivl num)
      (define tx (transform-thnk))
      (hash-ref! memo (vector outer-ivl num tx)
                 (λ ()
                   (define xs (sample-points outer-ivl inner-ivl num tx))
                   (define ys (map* f xs))
                   (define rys (filter rational? ys))
                   (define-values (y-min y-max)
                     (cond [(empty? rys)  (values #f #f)]
                           [else  (values (apply min* rys) (apply max* rys))]))
                   (sample xs ys
                           (maybe-inexact->exact y-min)
                           (maybe-inexact->exact y-max)))))))

(defproc (make-2d-function->sampler [transform-x-thnk (-> axis-transform/c)]
                                    [transform-y-thnk (-> axis-transform/c)]
                                    ) (-> (real? real? . -> . real?)
                                          (vector/c ivl? ivl?)
                                           2d-sampler/c)
  (λ (g inner-rect)
    (define f (λ (x y) (with-handlers ([exn:fail?  (λ (_) +nan.0)]) (g x y))))
    (define memo (make-hash))
    (λ (outer-rect nums)
      (define tx (transform-x-thnk))
      (define ty (transform-y-thnk))
      (hash-ref! memo (vector outer-rect nums tx ty)
                 (λ ()
                   (match-define (vector outer-x-ivl outer-y-ivl) outer-rect)
                   (match-define (vector inner-x-ivl inner-y-ivl) inner-rect)
                   (match-define (vector x-num y-num) nums)
                   (define xs (sample-points outer-x-ivl inner-x-ivl x-num tx))
                   (define ys (sample-points outer-y-ivl inner-y-ivl y-num ty))
                   (define z-min #f)
                   (define z-max #f)
                   (define zss (for/vector #:length (length ys) ([y  (in-list ys)])
                                 (for/vector #:length (length xs) ([x  (in-list xs)])
                                   (let ([z  (f x y)])
                                     (when (rational? z)
                                       (unless (and z-min (z . >= . z-min)) (set! z-min z))
                                       (unless (and z-max (z . <= . z-max)) (set! z-max z)))
                                     z))))
                   (2d-sample xs ys zss
                              (maybe-inexact->exact z-min)
                              (maybe-inexact->exact z-max)))))))

(defproc (make-3d-function->sampler [transform-x-thnk (-> axis-transform/c)]
                                    [transform-y-thnk (-> axis-transform/c)]
                                    [transform-z-thnk (-> axis-transform/c)]
                                    ) (-> (real? real? real? . -> . real?)
                                          (vector/c ivl? ivl? ivl?)
                                          3d-sampler/c)
  (λ (g inner-rect)
    (define f (λ (x y z) (with-handlers ([exn:fail?  (λ (_) +nan.0)]) (g x y z))))
    (define memo (make-hash))
    (λ (outer-rect nums)
      (define tx (transform-x-thnk))
      (define ty (transform-y-thnk))
      (define tz (transform-z-thnk))
      (hash-ref! memo (vector outer-rect nums tx ty tz)
                 (λ ()
                   (match-define (vector outer-x-ivl outer-y-ivl outer-z-ivl) outer-rect)
                   (match-define (vector inner-x-ivl inner-y-ivl inner-z-ivl) inner-rect)
                   (match-define (vector x-num y-num z-num) nums)
                   (define xs (sample-points outer-x-ivl inner-x-ivl x-num tx))
                   (define ys (sample-points outer-y-ivl inner-y-ivl y-num ty))
                   (define zs (sample-points outer-z-ivl inner-z-ivl z-num tz))
                   (define d-min #f)
                   (define d-max #f)
                   (define dsss (for/vector #:length (length zs) ([z  (in-list zs)])
                                  (for/vector #:length (length ys) ([y  (in-list ys)])
                                    (for/vector #:length (length xs) ([x  (in-list xs)])
                                      (let ([d  (f x y z)])
                                        (when (rational? d)
                                          (unless (and d-min (d . >= . d-min)) (set! d-min d))
                                          (unless (and d-max (d . <= . d-max)) (set! d-max d)))
                                        d)))))
                   (3d-sample xs ys zs dsss
                              (maybe-inexact->exact d-min)
                              (maybe-inexact->exact d-max)))))))

(define-syntax-rule (for-2d-sample (xa xb ya yb z1 z2 z3 z4) sample expr ...)
  (let ()
    (match-define (2d-sample xs ys zss fz-min fz-max) sample)
    (define ya (first ys))
    (define zs0 (vector-ref zss 0))
    (for/fold ([ya ya] [zs0 zs0]) ([yb  (in-list (rest ys))]
                                   [zs1  (in-vector zss 1)])
      (define xa (first xs))
      (define z1 (vector-ref zs0 0))
      (define z4 (vector-ref zs1 0))
      (for/fold ([xa xa] [z1 z1] [z4 z4]) ([xb  (in-list (rest xs))]
                                           [z2  (in-vector zs0 1)]
                                           [z3  (in-vector zs1 1)])
        expr ...
        (values xb z2 z3))
      (values yb zs1))))

(define-syntax-rule (for-3d-sample (xa xb ya yb za zb d1 d2 d3 d4 d5 d6 d7 d8) sample expr ...)
  (let ()
    (match-define (3d-sample xs ys zs dsss fd-min fd-max) sample)
    (define za (first zs))
    (define dss0 (vector-ref dsss 0))
    (for/fold ([za za] [dss0 dss0]) ([zb  (in-list (rest zs))]
                                     [dss1  (in-vector dsss 1)])
      (define ya (first ys))
      (define ds00 (vector-ref dss0 0))
      (define ds10 (vector-ref dss1 0))
      (for/fold ([ya ya] [ds00 ds00] [ds10 ds10]) ([yb  (in-list (rest ys))]
                                                   [ds01  (in-vector dss0 1)]
                                                   [ds11  (in-vector dss1 1)])
        (define xa (first xs))
        (define d1 (vector-ref ds00 0))
        (define d4 (vector-ref ds01 0))
        (define d5 (vector-ref ds10 0))
        (define d8 (vector-ref ds11 0))
        (for/fold ([xa xa] [d1 d1] [d4 d4] [d5 d5] [d8 d8]) ([xb  (in-list (rest xs))]
                                                             [d2  (in-vector ds00 1)]
                                                             [d3  (in-vector ds01 1)]
                                                             [d6  (in-vector ds10 1)]
                                                             [d7  (in-vector ds11 1)])
          expr ...
          (values xb d2 d3 d6 d7))
        (values yb ds01 ds11))
      (values zb dss1))))

(defproc (sample-exact->inexact [s sample?]) sample?
  (match-define (sample xs ys y-min y-max) s)
  (sample (map exact->inexact xs) (map exact->inexact ys)
          y-min y-max))

(defproc (2d-sample-exact->inexact [s 2d-sample?]) 2d-sample?
  (match-define (2d-sample xs ys zss z-min z-max) s)
  (2d-sample (map exact->inexact xs) (map exact->inexact ys)
             (for/vector #:length (vector-length zss) ([zs  (in-vector zss)])
               (for/vector #:length (vector-length zs) ([z  (in-vector zs)])
                 (exact->inexact z)))
             z-min z-max))

(defproc (3d-sample-exact->inexact [s 3d-sample?]) 3d-sample?
  (match-define (3d-sample xs ys zs dsss d-min d-max) s)
  (3d-sample (map exact->inexact xs) (map exact->inexact ys) (map exact->inexact zs)
             (for/vector #:length (vector-length dsss) ([dss  (in-vector dsss)])
               (for/vector #:length (vector-length dss) ([ds  (in-vector dss)])
                 (for/vector #:length (vector-length ds) ([d  (in-vector ds)])
                   (exact->inexact d))))
             d-min d-max))

(defproc (flonum-ok-for-2d? [x-min rational?] [x-max rational?]
                            [y-min rational?] [y-max rational?]) boolean?
  (and (flonum-ok-for-range? x-min x-max 10000)
       (flonum-ok-for-range? y-min y-max 10000)))

(defproc (flonum-ok-for-3d? [x-min rational?] [x-max rational?]
                            [y-min rational?] [y-max rational?]
                            [z-min rational?] [z-max rational?]) boolean?
  (and (flonum-ok-for-range? x-min x-max 10000)
       (flonum-ok-for-range? y-min y-max 10000)
       (flonum-ok-for-range? z-min z-max 10000)))

(defproc (flonum-ok-for-4d? [x-min rational?] [x-max rational?]
                            [y-min rational?] [y-max rational?]
                            [z-min rational?] [z-max rational?]
                            [d-min rational?] [d-max rational?]) boolean?
  (and (flonum-ok-for-range? x-min x-max 10000)
       (flonum-ok-for-range? y-min y-max 10000)
       (flonum-ok-for-range? z-min z-max 10000)
       (flonum-ok-for-range? d-min d-max 10000)))
