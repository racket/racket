#lang typed/racket/base

(require racket/match racket/math
         (only-in racket/unsafe/ops unsafe-flvector-ref)
         "flonum.rkt"
         "flomap-struct.rkt")

(provide flomap-gaussian-blur-x flomap-gaussian-blur-y flomap-gaussian-blur
         flomap-box-blur-x flomap-box-blur-y flomap-box-blur
         flomap-blur-x flomap-blur-y flomap-blur)

;; ===================================================================================================
;; Gaussian blur

(: flomap-gaussian-blur (case-> (flomap Real -> flomap)
                                (flomap Real Real -> flomap)))
(define flomap-gaussian-blur
  (case-lambda
    [(fm xσ)  (flomap-gaussian-blur fm xσ xσ)]
    [(fm xσ yσ)
     (flomap-gaussian-blur-y (flomap-gaussian-blur-x fm (abs (real->double-flonum xσ)))
                             (abs (real->double-flonum yσ)))]))

(: flomap-gaussian-blur-x (flomap Real -> flomap))
(define (flomap-gaussian-blur-x fm σ*)
  (cond
    [(σ* . = . 0)  fm]
    [else
     (define σ (abs (real->double-flonum σ*)))
     (define dx-min (fl->fx (floor (* (- 3.0) σ))))
     (define dx-max (fx+ 1 (fl->fx (ceiling (* 3.0 σ)))))
     (define ss (gaussian-kernel-1d dx-min dx-max σ))
     
     (match-define (flomap vs c w h) fm)
     (inline-build-flomap
      c w h
      (λ (k x y i)
        (define dx-start (fx- (fxmax (fx+ x dx-min) 0) x))
        (define dx-end (fx- (fxmin (fx+ x dx-max) w) x))
        (define j (fx+ i (fx* c dx-start)))
        (let: src-loop : Float ([sum : Float  0.0] [dx : Fixnum  dx-start] [j : Fixnum  j])
          (cond [(dx . fx< . dx-end)  (define s (unsafe-flvector-ref ss (fx- dx dx-min)))
                                      (src-loop (+ sum (* s (unsafe-flvector-ref vs j)))
                                                (fx+ dx 1)
                                                (fx+ j c))]
                [else  sum]))))]))

(: flomap-gaussian-blur-y (flomap Real -> flomap))
(define (flomap-gaussian-blur-y fm σ*)
  (cond
    [(σ* . = . 0)  fm]
    [else
     (define σ (abs (real->double-flonum σ*)))
     (define dy-min (fl->fx (floor (* (- 3.0) σ))))
     (define dy-max (fx+ 1 (fl->fx (ceiling (* 3.0 σ)))))
     (define ss (gaussian-kernel-1d dy-min dy-max σ))
     
     (match-define (flomap vs c w h) fm)
     (define cw (* c w))
     (inline-build-flomap
      c w h
      (λ (k x y i)
        (define dy-start (fx- (fxmax (fx+ y dy-min) 0) y))
        (define dy-end (fx- (fxmin (fx+ y dy-max) h) y))
        (define j (fx+ i (fx* cw dy-start)))
        (let: src-loop : Float ([sum : Float  0.0] [dy : Fixnum  dy-start] [j : Fixnum  j])
          (cond [(dy . fx< . dy-end)  (define s (unsafe-flvector-ref ss (fx- dy dy-min)))
                                      (src-loop (+ sum (* s (unsafe-flvector-ref vs j)))
                                                (fx+ dy 1)
                                                (fx+ j cw))]
                [else  sum]))))]))

(: gaussian-kernel-1d (Fixnum Fixnum Float -> FlVector))
(define (gaussian-kernel-1d mn mx σ)
  (define n (fx- mx mn))
  (define ys (make-flvector n))
  (define sum
    (let: loop : Float ([i : Fixnum  0] [sum : Float  0.0])
      (cond [(i . fx< . n)  (define v (flgaussian (fx->fl (fx+ i mn)) σ))
                            (flvector-set! ys i v)
                            (loop (fx+ i 1) (+ sum v))]
            [else  sum])))
  (let: loop : FlVector ([i : Integer  0])
    (cond [(i . fx< . n)  (flvector-set! ys i (/ (flvector-ref ys i) sum))
                          (loop (fx+ i 1))]
          [else  ys])))


;; ===================================================================================================
;; Integral images

(: flomap-integral (flomap -> flomap))
(define (flomap-integral fm)
  (match-define (flomap vs c w h) fm)
  (define w+1 (fx+ w 1))
  (define c*w+1 (fx* c w+1))
  (define h+1 (fx+ h 1))
  (define new-vs (make-flvector (* c w+1 h+1)))
  (let: y-loop : Void ([y : Nonnegative-Fixnum  0] [i : Nonnegative-Fixnum  0])
    (when (y . fx< . h)
      (let: x-loop : Void ([x : Nonnegative-Fixnum  0] [i : Nonnegative-Fixnum  i])
        (cond [(x . fx< . w)
               (let: k-loop : Void ([k : Nonnegative-Fixnum  0] [i : Nonnegative-Fixnum  i])
                 (cond [(k . fx< . c)
                        (define j00 (coords->index c w+1 k x y))
                        (define j01 (fx+ j00 c*w+1))
                        (flvector-set! new-vs (fx+ j01 c)
                                       (- (+ (flvector-ref vs i)
                                             (flvector-ref new-vs j01)
                                             (flvector-ref new-vs (fx+ j00 c)))
                                          (flvector-ref new-vs j00)))
                        (k-loop (fx+ k 1) (fx+ i 1))]
                       [else  (x-loop (fx+ x 1) i)]))]
              [else  (y-loop (fx+ y 1) i)]))))
  (flomap new-vs c w+1 h+1))

(: flomap-integral-x (flomap -> flomap))
(define (flomap-integral-x fm)
  (match-define (flomap vs c w h) fm)
  (define w+1 (fx+ w 1))
  (define new-vs (make-flvector (* c w+1 h)))
  (let: y-loop : Void ([y : Nonnegative-Fixnum  0] [i : Nonnegative-Fixnum  0])
    (when (y . fx< . h)
      (let: x-loop : Void ([x : Nonnegative-Fixnum  0] [i : Nonnegative-Fixnum  i])
        (cond [(x . fx< . w)
               (let: k-loop : Void ([k : Nonnegative-Fixnum  0] [i : Nonnegative-Fixnum  i])
                 (cond [(k . fx< . c)
                        (define j0 (coords->index c w+1 k x y))
                        (define j1 (fx+ j0 c))
                        (flvector-set! new-vs j1 (+ (flvector-ref vs i)
                                                    (flvector-ref new-vs j0)))
                        (k-loop (fx+ k 1) (fx+ i 1))]
                       [else  (x-loop (fx+ x 1) i)]))]
              [else  (y-loop (fx+ y 1) i)]))))
  (flomap new-vs c w+1 h))

(: flomap-integral-y (flomap -> flomap))
(define (flomap-integral-y fm)
  (match-define (flomap vs c w h) fm)
  (define h+1 (fx+ h 1))
  (define cw (fx* c w))
  (define new-vs (make-flvector (* c w h+1)))
  (let: y-loop : Void ([y : Nonnegative-Fixnum  0])
    (when (y . fx< . h)
      (let: x-loop : Void ([x : Nonnegative-Fixnum  0])
        (cond [(x . fx< . w)
               (let: k-loop : Void ([k : Nonnegative-Fixnum  0])
                 (cond [(k . fx< . c)
                        (define j0 (coords->index c w k x y))
                        (define j1 (fx+ j0 cw))
                        (flvector-set! new-vs j1 (+ (flvector-ref vs j0)
                                                    (flvector-ref new-vs j0)))
                        (k-loop (fx+ k 1))]
                       [else  (x-loop (fx+ x 1))]))]
              [else  (y-loop (fx+ y 1))]))))
  (flomap new-vs c w h+1))

(: raw-flomap-integral-sum (FlVector Integer Integer Integer
                                     Integer Integer Integer Integer Integer
                                     -> Float))
(define (raw-flomap-integral-sum vs c w h k x-start y-start x-end y-end)
  (define w-1 (fx- w 1))
  (define h-1 (fx- h 1))
  (define x1 (fxmax 0 (fxmin x-start w-1)))
  (define x2 (fxmax 0 (fxmin x-end w-1)))
  (define y1 (fxmax 0 (fxmin y-start h-1)))
  (define y2 (fxmax 0 (fxmin y-end h-1)))
  (- (+ (flvector-ref vs (coords->index c w k x1 y1))
        (flvector-ref vs (coords->index c w k x2 y2)))
     (+ (flvector-ref vs (coords->index c w k x1 y2))
        (flvector-ref vs (coords->index c w k x2 y1)))))

(: raw-flomap-integral-x-sum (FlVector Integer Integer
                                       Integer Integer Integer Integer -> Float))
(define (raw-flomap-integral-x-sum vs c w k x-start x-end y)
  (define w-1 (fx- w 1))
  (define x1 (fxmax 0 (fxmin x-start w-1)))
  (define x2 (fxmax 0 (fxmin x-end w-1)))
  (- (flvector-ref vs (coords->index c w k x2 y))
     (flvector-ref vs (coords->index c w k x1 y))))

(: raw-flomap-integral-y-sum (FlVector Integer Integer Integer
                                       Integer Integer Integer Integer -> Float))
(define (raw-flomap-integral-y-sum vs c w h k x y-start y-end)
  (define h-1 (fx- h 1))
  (define y1 (fxmax 0 (fxmin y-start h-1)))
  (define y2 (fxmax 0 (fxmin y-end h-1)))
  (- (flvector-ref vs (coords->index c w k x y2))
     (flvector-ref vs (coords->index c w k x y1))))

;; ===================================================================================================
;; Box blur

(: flomap-box-blur (case-> (flomap Real -> flomap)
                           (flomap Real Real -> flomap)))
(define flomap-box-blur
  (case-lambda
    [(fm xr)  (flomap-box-blur fm xr xr)]
    [(fm xr yr)
     (let ([xr  (abs (real->double-flonum xr))]
           [yr  (abs (real->double-flonum yr))])
       (cond [(and (integer? xr) (integer? yr))
              (let ([xr  (fl->fx xr)] [yr  (fl->fx yr)])
                (with-asserts ([xr  nonnegative-fixnum?] [yr  nonnegative-fixnum?])
                  (flomap-box-blur/int fm xr yr)))]
             [else
              (flomap-box-blur-y (flomap-box-blur-x fm xr) yr)]))]))

(: flomap-box-blur-x (flomap Real -> flomap))
(define (flomap-box-blur-x fm r*)
  (define r (abs (real->double-flonum r*)))
  (cond
    [(integer? r)  (let ([r  (fl->fx r)])
                     (with-asserts ([r  nonnegative-fixnum?])
                       (flomap-box-blur-x/int fm r)))]
    [else
     (define r1 (fl->fx (floor r)))
     (define r2 (fx+ r1 1))
     (define s (+ 1.0 (* 2.0 r)))
     (define s1 (+ 1.0 (* 2.0 r1)))
     (define s2 (+ 1.0 (* 2.0 r2)))
     (define α (/ (- (sqr s2) (sqr s)) (- (sqr s2) (sqr s1))))
     (define norm1 (/ α s1))
     (define norm2 (/ (- 1.0 α) s2))
     (define r1+1 (fx+ r1 1))
     (define r2+1 (fx+ r2 1))
     (match-define (flomap _ c w h) fm)
     (match-define (flomap int-vs int-c int-w int-h) (flomap-integral-x fm))
     (inline-build-flomap
      c w h
      (λ (k x y _i)
        (+ (* norm1 (raw-flomap-integral-x-sum int-vs int-c int-w k (fx- x r1) (fx+ x r1+1) y))
           (* norm2 (raw-flomap-integral-x-sum int-vs int-c int-w k (fx- x r2) (fx+ x r2+1) y))
           )))]))

(: flomap-box-blur-y (flomap Real -> flomap))
(define (flomap-box-blur-y fm r*)
  (define r (abs (real->double-flonum r*)))
  (cond
    [(integer? r)  (let ([r  (fl->fx r)])
                     (with-asserts ([r  nonnegative-fixnum?])
                       (flomap-box-blur-y/int fm r)))]
    [else
     (define r1 (fl->fx (floor r)))
     (define r2 (fx+ r1 1))
     (define s (+ 1.0 (* 2.0 r)))
     (define s1 (+ 1.0 (* 2.0 r1)))
     (define s2 (+ 1.0 (* 2.0 r2)))
     (define α (/ (- (sqr s2) (sqr s)) (- (sqr s2) (sqr s1))))
     (define norm1 (/ α s1))
     (define norm2 (/ (- 1.0 α) s2))
     (define r1+1 (fx+ r1 1))
     (define r2+1 (fx+ r2 1))
     (match-define (flomap _ c w h) fm)
     (match-define (flomap int-vs int-c int-w int-h) (flomap-integral-y fm))
     (inline-build-flomap
      c w h
      (λ (k x y _i)
        (+ (* norm1 (raw-flomap-integral-y-sum int-vs int-c int-w int-h k x (fx- y r1) (fx+ y r1+1)))
           (* norm2 (raw-flomap-integral-y-sum int-vs int-c int-w int-h k x (fx- y r2) (fx+ y r2+1)))
           )))]))

(: flomap-box-blur/int (flomap Nonnegative-Fixnum Nonnegative-Fixnum -> flomap))
(define (flomap-box-blur/int fm xr yr)
  (define norm (/ 1.0 (* (+ 1.0 (* 2.0 xr)) (+ 1.0 (* 2.0 yr)))))
  (define xr+1 (fx+ xr 1))
  (define yr+1 (fx+ yr 1))
  (match-define (flomap _ c w h) fm)
  (match-define (flomap int-vs int-c int-w int-h) (flomap-integral fm))
  (inline-build-flomap
   c w h
   (λ (k x y _i)
     (* norm (raw-flomap-integral-sum int-vs int-c int-w int-h k
                                      (fx- x xr) (fx- y yr)
                                      (fx+ x xr+1) (fx+ y yr+1))))))

(: flomap-box-blur-x/int (flomap Nonnegative-Fixnum -> flomap))
(define (flomap-box-blur-x/int fm r)
  (define norm (/ 1.0 (+ 1.0 (* 2.0 r))))
  (define r+1 (fx+ r 1))
  (match-define (flomap _ c w h) fm)
  (match-define (flomap int-vs int-c int-w int-h) (flomap-integral-x fm))
  (inline-build-flomap
   c w h
   (λ (k x y _i)
     (* norm (raw-flomap-integral-x-sum int-vs int-c int-w k (fx- x r) (fx+ x r+1) y)))))

(: flomap-box-blur-y/int (flomap Nonnegative-Fixnum -> flomap))
(define (flomap-box-blur-y/int fm r)
  (define norm (/ 1.0 (+ 1.0 (* 2.0 r))))
  (define r+1 (fx+ r 1))
  (match-define (flomap _ c w h) fm)
  (match-define (flomap int-vs int-c int-w int-h) (flomap-integral-y fm))
  (inline-build-flomap
   c w h
   (λ (k x y _i)
     (* norm (raw-flomap-integral-y-sum int-vs int-c int-w int-h k x (fx- y r) (fx+ y r+1))))))

;; ===================================================================================================
;; Default blur

(: box-radius->variance (Float -> Float))
(define (box-radius->variance r)
  (* 1/12 (sqr (+ 1 (* 2 r)))))

(: variance->box-radius (Float -> Float))
(define (variance->box-radius σ^2)
  (* 1/2 (- (flsqrt (* 12 σ^2)) 1)))

(: flomap-blur (case-> (flomap Real -> flomap)
                       (flomap Real Real -> flomap)))
(define flomap-blur
  (case-lambda
    [(fm σ)  (flomap-blur fm σ σ)]
    [(fm xσ yσ)
     (let ([xσ  (abs (real->double-flonum xσ))]
           [yσ  (abs (real->double-flonum yσ))])
       (cond
         [(and (xσ . >= . 1.5) (yσ . >= . 1.5))
          (define xσ^2 (sqr xσ))
          (define yσ^2 (sqr yσ))
          (define xr (floor (variance->box-radius (/ xσ^2 3.0))))
          (define yr (floor (variance->box-radius (/ yσ^2 3.0))))
          (flomap-box-blur (flomap-box-blur (flomap-box-blur fm xr yr) xr yr)
                           (variance->box-radius (- xσ^2 (* 2.0 (box-radius->variance xr))))
                           (variance->box-radius (- yσ^2 (* 2.0 (box-radius->variance yr)))))]
         [else
          (flomap-blur-x (flomap-blur-y fm yσ) xσ)]))]))

(: make-flomap-blur-dimension
   ((flomap Float -> flomap) (flomap Float -> flomap) -> (flomap Float -> flomap)))
(define ((make-flomap-blur-dimension gaussian-blur box-blur) fm σ)
  (cond
    [(σ . = . 0.0)  fm]
    [(σ . < . 1.5)  (gaussian-blur fm σ)]
    [else
     (define σ^2 (sqr σ))
     (define r (floor (variance->box-radius (/ σ^2 3.0))))
     (box-blur (box-blur (box-blur fm r) r)
               (variance->box-radius (- σ^2 (* 2.0 (box-radius->variance r)))))]))

(define flomap-blur-x (make-flomap-blur-dimension flomap-gaussian-blur-x flomap-box-blur-x))
(define flomap-blur-y (make-flomap-blur-dimension flomap-gaussian-blur-y flomap-box-blur-y))
