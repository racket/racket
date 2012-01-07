#lang racket/base

(require racket/flonum racket/draw racket/match racket/math racket/contract racket/class
         "unsafe.rkt"
         "flomap.rkt")

(provide
 (contract-out
  ;; type, contructors and accessors
  (struct deep-flomap ([argb flomap?] [z flomap?]))
  [flomap->deep-flomap  (flomap? . -> . deep-flomap?)]
  [bitmap->deep-flomap  ((is-a?/c bitmap%) . -> . deep-flomap?)]
  [deep-flomap-width    (deep-flomap? . -> . (fx>=/c 0))]
  [deep-flomap-height   (deep-flomap? . -> . (fx>=/c 0))]
  [deep-flomap-z-min    (deep-flomap? . -> . flonum?)]
  [deep-flomap-z-max    (deep-flomap? . -> . flonum?)]
  [deep-flomap-size     (deep-flomap? . -> . (values (fx>=/c 0) (fx>=/c 0)))]
  [deep-flomap-alpha    (deep-flomap? . -> . flomap?)]
  [deep-flomap-rgb      (deep-flomap? . -> . flomap?)]
  ;; sizing
  [deep-flomap-inset   (case-> (deep-flomap? fixnum? . -> . deep-flomap?)
                               (deep-flomap? fixnum? fixnum? . -> . deep-flomap?)
                               (deep-flomap? fixnum? fixnum? fixnum? fixnum? . -> . deep-flomap?))]
  [deep-flomap-trim    (deep-flomap? . -> . deep-flomap?)]
  [deep-flomap-scale   (case-> (deep-flomap? (>/c 0.0) . -> . deep-flomap?)
                               (deep-flomap? (>/c 0.0) (>/c 0.0) (>/c 0.0) . -> . deep-flomap?))]
  [deep-flomap-resize  (deep-flomap? (or/c (>/c 0.0) #f) (or/c (>/c 0.0) #f)
                                     (or/c real? #f) (or/c real? #f)
                                     . -> . deep-flomap?)]
  ;; z-adjusting
  [deep-flomap-scale-z   (deep-flomap? (or/c flomap? real?) . -> . deep-flomap?)]
  [deep-flomap-smooth-z  (deep-flomap? real? . -> . deep-flomap?)]
  [deep-flomap-raise     (deep-flomap? (or/c flomap? real?) . -> . deep-flomap?)]
  [deep-flomap-tilt      (deep-flomap? real? real? real? real? . -> . deep-flomap?)]
  [deep-flomap-emboss    (deep-flomap? real? real? . -> . deep-flomap?)]
  [deep-flomap-bulge             (deep-flomap? (flonum? flonum? . -> . real?) . -> . deep-flomap?)]
  [deep-flomap-bulge-round       (deep-flomap? real? . -> . deep-flomap?)]
  [deep-flomap-bulge-round-rect  (deep-flomap? real? . -> . deep-flomap?)]
  [deep-flomap-bulge-spheroid    (deep-flomap? real? . -> . deep-flomap?)]
  [deep-flomap-bulge-horizontal  (deep-flomap? real? . -> . deep-flomap?)]
  [deep-flomap-bulge-vertical    (deep-flomap? real? . -> . deep-flomap?)]
  [deep-flomap-bulge-ripple      (deep-flomap? real? real? . -> . deep-flomap?)]
  ;; combining
  [deep-flomap-pin   (->* [deep-flomap? real? real? deep-flomap? real? real?]
                          [#:z-mode (one-of/c 'place 'replace 'add 'blend)]
                          deep-flomap?)]
  [deep-flomap-pin*  (->* [real? real? real? real? deep-flomap?]
                          [#:z-mode (one-of/c 'place 'replace 'add 'blend)]
                          #:rest (listof deep-flomap?)
                          deep-flomap?)]
  [deep-flomap-lt-superimpose  deep-flomap-superimpose/c]
  [deep-flomap-lc-superimpose  deep-flomap-superimpose/c]
  [deep-flomap-lb-superimpose  deep-flomap-superimpose/c]
  [deep-flomap-ct-superimpose  deep-flomap-superimpose/c]
  [deep-flomap-cc-superimpose  deep-flomap-superimpose/c]
  [deep-flomap-cb-superimpose  deep-flomap-superimpose/c]
  [deep-flomap-rt-superimpose  deep-flomap-superimpose/c]
  [deep-flomap-rc-superimpose  deep-flomap-superimpose/c]
  [deep-flomap-rb-superimpose  deep-flomap-superimpose/c]
  [deep-flomap-vl-append  deep-flomap-append/c]
  [deep-flomap-vc-append  deep-flomap-append/c]
  [deep-flomap-vr-append  deep-flomap-append/c]
  [deep-flomap-ht-append  deep-flomap-append/c]
  [deep-flomap-hc-append  deep-flomap-append/c]
  [deep-flomap-hb-append  deep-flomap-append/c]
  [deep-flomap-superimpose/c  contract?]
  [deep-flomap-append/c  contract?]
  ))

(struct deep-flomap (argb z)
  #:guard
  (λ (argb-fm z-fm name)
    (match-define (flomap _ 4 w h) argb-fm)
    (match-define (flomap _ 1 zw zh) z-fm)
    (unless (and (= w zw) (= h zh))
      (error 'deep-flomap
             "expected flomaps of equal dimension; given dimensions ~e×~e and ~e×~e" w h zw zh))
    (values argb-fm z-fm)))

(define (flomap->deep-flomap argb-fm)
  (match-define (flomap _ 4 w h) argb-fm)
  (deep-flomap argb-fm (make-flomap 1 w h)))

(define (bitmap->deep-flomap bm)
  (flomap->deep-flomap (bitmap->flomap bm)))

(define (deep-flomap-width dfm)
  (flomap-width (deep-flomap-argb dfm)))

(define (deep-flomap-height dfm)
  (flomap-height (deep-flomap-argb dfm)))

(define (deep-flomap-z-min dfm)
  (flomap-min-value (deep-flomap-z dfm)))

(define (deep-flomap-z-max dfm)
  (flomap-max-value (deep-flomap-z dfm)))

(define (deep-flomap-size dfm)
  (values (deep-flomap-width dfm) (deep-flomap-height dfm)))

(define (deep-flomap-alpha dfm)
  (flomap-ref-component (deep-flomap-argb dfm) 0))

(define (deep-flomap-rgb dfm)
  (flomap-drop-components (deep-flomap-argb dfm) 1))

;; ===================================================================================================
;; Z adjusters

(define (deep-flomap-scale-z dfm z)
  (match-define (deep-flomap argb-fm z-fm) dfm)
  (deep-flomap argb-fm (fm* z-fm z)))

(define (deep-flomap-smooth-z dfm σ)
  (let ([σ  (exact->inexact σ)])
    (match-define (deep-flomap argb-fm z-fm) dfm)
    (define new-z-fm (flomap-blur z-fm σ))
    (deep-flomap argb-fm new-z-fm)))

;; deep-flomap-raise and everything derived from it observe an invariant:
;; when z is added, added z must be 0.0 everywhere alpha is 0.0

(define (deep-flomap-raise dfm z)
  (match-define (deep-flomap argb-fm z-fm) dfm)
  (define alpha-fm (deep-flomap-alpha dfm))
  (deep-flomap argb-fm (fm+ z-fm (fm* alpha-fm z))))

(define (deep-flomap-emboss dfm xy-amt z-amt)
  (let ([σ      (/ xy-amt 3.0)])
    (define z-fm (flomap-normalize (deep-flomap-alpha dfm)))
    (define new-z-fm (fm* (flomap-blur z-fm σ)
                          (exact->inexact z-amt)))
    (deep-flomap-raise dfm new-z-fm)))

(define-syntax-rule (unsafe-deep-flomap-bulge dfm f)
  (let ()
    (define-values (w h) (deep-flomap-size dfm))
    (define half-x-size (unsafe-fl- (unsafe-fl* 0.5 (unsafe-fx->fl w)) 0.5))
    (define half-y-size (unsafe-fl- (unsafe-fl* 0.5 (unsafe-fx->fl h)) 0.5))
    (define z-fm
      (unsafe-build-flomap
       1 w h
       (λ (_ x y)
         (f (unsafe-fl- (unsafe-fl/ (unsafe-fx->fl x) half-x-size) 1.0)
            (unsafe-fl- (unsafe-fl/ (unsafe-fx->fl y) half-y-size) 1.0)))))
    (deep-flomap-raise dfm z-fm)))

(define (deep-flomap-bulge dfm f)
  (unsafe-deep-flomap-bulge dfm (λ (cx cy) (exact->inexact (f cx cy)))))

(define (deep-flomap-tilt dfm left-z-amt top-z-amt right-z-amt bottom-z-amt)
  (let ([l  (exact->inexact left-z-amt)]
        [t  (exact->inexact top-z-amt)]
        [r  (exact->inexact right-z-amt)]
        [b  (exact->inexact bottom-z-amt)])
    (define (f x y)
      (define α (unsafe-fl/ (unsafe-fl+ x 1.0) 2.0))
      (define β (unsafe-fl/ (unsafe-fl+ y 1.0) 2.0))
      (unsafe-flsum (unsafe-fl* (unsafe-fl- 1.0 α) l) (unsafe-fl* α r)
                    (unsafe-fl* (unsafe-fl- 1.0 β) t) (unsafe-fl* β b)))
    (unsafe-deep-flomap-bulge dfm f)))

(define (deep-flomap-bulge-round dfm z-amt)
  (let ([z-amt  (exact->inexact z-amt)])
    (define (f x y)
      (define d^2 (unsafe-fl+ (unsafe-fl* x x) (unsafe-fl* y y)))
      (unsafe-fl* z-amt (unsafe-flsqrt (unsafe-fl/ (unsafe-fl- 2.0 d^2) 2.0))))
    (unsafe-deep-flomap-bulge dfm f)))

(define (deep-flomap-bulge-round-rect dfm z-amt)
  (let ([z-amt  (exact->inexact z-amt)])
    (define (f x y)
      (unsafe-fl* z-amt (unsafe-flsqrt
                         (unsafe-fl* (unsafe-fl- 1.0 (unsafe-fl* x x))
                                     (unsafe-fl- 1.0 (unsafe-fl* y y))))))
    (unsafe-deep-flomap-bulge dfm f)))

(define (deep-flomap-bulge-spheroid dfm z-amt)
  (let ([z-amt  (exact->inexact z-amt)])
    (define (f x y)
      (define d^2 (unsafe-fl+ (unsafe-fl* x x) (unsafe-fl* y y)))
      (cond [(d^2 . unsafe-fl< . 1.0)
             (unsafe-fl* z-amt (unsafe-flsqrt (unsafe-fl- 1.0 d^2)))]
            [else  0.0]))
    (unsafe-deep-flomap-bulge dfm f)))

(define (deep-flomap-bulge-horizontal dfm z-amt)
  (let ([z-amt  (exact->inexact z-amt)])
    (define (f x _)
      (define d^2 (unsafe-fl* x x))
      (unsafe-fl* z-amt (unsafe-flsqrt (unsafe-fl- 1.0 d^2))))
    (unsafe-deep-flomap-bulge dfm f)))

(define (deep-flomap-bulge-vertical dfm z-amt)
  (let ([z-amt  (exact->inexact z-amt)])
    (define (f _ y)
      (define d^2 (unsafe-fl* y y))
      (unsafe-fl* z-amt (unsafe-flsqrt (unsafe-fl- 1.0 d^2))))
    (unsafe-deep-flomap-bulge dfm f)))

(define (deep-flomap-bulge-ripple dfm freq z-amt)
  (let ([freq   (exact->inexact freq)]
        [z-amt  (exact->inexact z-amt)])
    (define (f x y)
      (define d^2 (unsafe-fl+ (unsafe-fl* x x) (unsafe-fl* y y)))
      (define d (unsafe-flproduct freq pi (unsafe-flsqrt d^2)))
      (unsafe-flproduct z-amt 0.5 (unsafe-fl- 1.0 (unsafe-flcos d))))
    (unsafe-deep-flomap-bulge dfm f)))

;; ===================================================================================================
;; Sizing

(define deep-flomap-inset
  (case-lambda
    [(dfm amt)
     (deep-flomap-inset dfm amt amt amt amt)]
    [(dfm h-amt v-amt)
     (deep-flomap-inset dfm h-amt v-amt h-amt v-amt)]
    [(dfm l-amt t-amt r-amt b-amt)
     (match-define (deep-flomap argb-fm z-fm) dfm)
     (deep-flomap (flomap-inset argb-fm l-amt t-amt r-amt b-amt)
                  (flomap-inset z-fm l-amt t-amt r-amt b-amt))]))

(define (deep-flomap-trim dfm)
  (define-values (w h) (deep-flomap-size dfm))
  (define-values (_k-min x-min y-min _k-max x-max y-max)
    (flomap-nonzero-rect (deep-flomap-alpha dfm)))
  (deep-flomap-inset dfm (- x-min) (- y-min) (- x-max w) (- y-max h)))

(define deep-flomap-scale
  (case-lambda
    [(dfm scale)
     (match-define (deep-flomap argb-fm z-fm) (deep-flomap-scale-z dfm scale))
     (deep-flomap (flomap-scale argb-fm scale)
                  (flomap-scale z-fm scale))]
    [(dfm x-scale y-scale z-scale)
     (match-define (deep-flomap argb-fm z-fm) (deep-flomap-scale-z dfm z-scale))
     (deep-flomap (flomap-scale argb-fm x-scale y-scale)
                  (flomap-scale z-fm x-scale y-scale))]))

(define (deep-flomap-resize dfm width height z-min z-max)
  (match-define (deep-flomap argb-fm z-fm) dfm)
  (define new-z-fm
    (cond [(or z-min z-max)
           (let ([z-min  (if z-min z-min (flomap-min-value z-fm))]
                 [z-max  (if z-max z-max (flomap-max-value z-fm))])
             (fm+ (fm* (flomap-normalize z-fm) (- z-max z-min)) z-min))]
          [else  z-fm]))
  (deep-flomap (flomap-resize argb-fm width height)
               (flomap-resize new-z-fm width height)))

;; ===================================================================================================
;; Combining

(define (deep-flomap-pin dfm1 x1 y1 dfm2 x2 y2 #:z-mode [z-mode 'blend])
  (cond
    [(not (and (zero? x2) (zero? y2)))
     (deep-flomap-pin dfm1 (- x1 x2) (- y1 y2) dfm2 0 0 #:z-mode z-mode)]
    [else
     (define-values (w1 h1) (deep-flomap-size dfm1))
     (define-values (w2 h2) (deep-flomap-size dfm2))
     
     ;; dfm1 and dfm2 offsets, in final image coordinates
     (define dx1 (inexact->exact (round (max 0 (- x1)))))
     (define dy1 (inexact->exact (round (max 0 (- y1)))))
     (define dx2 (inexact->exact (round (max 0 x1))))
     (define dy2 (inexact->exact (round (max 0 y1))))
     ;; final image size
     (define w (max (+ dx1 w1) (+ dx2 w2)))
     (define h (max (+ dy1 h1) (+ dy2 h2)))
     
     (case z-mode
       [(place)  (deep-flomap-superimpose/place w h dfm1 dx1 dy1 w1 h1 dfm2 dx2 dy2 w2 h2)]
       [(blend)  (deep-flomap-superimpose/blend w h dfm1 dx1 dy1 w1 h1 dfm2 dx2 dy2 w2 h2)]
       [(replace add)  (deep-flomap-superimpose/replace w h
                                                        dfm1 dx1 dy1 w1 h1
                                                        dfm2 dx2 dy2 w2 h2 z-mode)])]))

(define (deep-flomap-superimpose/replace w h dfm1 dx1 dy1 w1 h1 dfm2 dx2 dy2 w2 h2 z-mode)
  (match-define (deep-flomap argb1-fm z1-fm) dfm1)
  (match-define (deep-flomap argb2-fm z2-fm) dfm2)
  (define argb1-vs (flomap-values argb1-fm))
  (define argb2-vs (flomap-values argb2-fm))
  (define z1-vs (flomap-values z1-fm))
  (define z2-vs (flomap-values z2-fm))
  
  (define-syntax-rule (get-argbz-pixel argb-vs z-vs dx dy w h x y)
    (let ([x  (unsafe-fx- x dx)]
          [y  (unsafe-fx- y dy)])
      (cond [(and (x . unsafe-fx>= . 0) (x . unsafe-fx< . w)
                  (y . unsafe-fx>= . 0) (y . unsafe-fx< . h))
             (define i (unsafe-fx+ x (unsafe-fx* y w)))
             (define-values (a r g b) (unsafe-flvector-4ref argb-vs (unsafe-fx* 4 i)))
             (define z (unsafe-flvector-ref z-vs i))
             (values a r g b z)]
            [else
             (values 0.0 0.0 0.0 0.0 0.0)])))
  
  (define argb-vs (make-flvector (* 4 w h)))
  (define z-vs (make-flvector (* w h)))
  (for* ([y  (in-range h)] [x  (in-range w)])
    (define-values (a1 r1 g1 b1 z1) (get-argbz-pixel argb1-vs z1-vs dx1 dy1 w1 h1 x y))
    (define-values (a2 r2 g2 b2 z2) (get-argbz-pixel argb2-vs z2-vs dx2 dy2 w2 h2 x y))
    
    (define i (unsafe-fx+ x (unsafe-fx* y w)))
    (unsafe-flvector-4set! argb-vs (unsafe-fx* 4 i)
                           (unsafe-fl-alpha-blend a1 a2 a2)
                           (unsafe-fl-alpha-blend r1 r2 a2)
                           (unsafe-fl-alpha-blend g1 g2 a2)
                           (unsafe-fl-alpha-blend b1 b2 a2))
    (unsafe-flvector-set! z-vs i
                          (case z-mode
                            [(replace)  (unsafe-fl-alpha-blend z1 z2 a2)]
                            [else       (unsafe-fl+ z1 z2)])))
  
  (deep-flomap (flomap argb-vs 4 w h)
               (flomap z-vs 1 w h)))

(define (deep-flomap-superimpose/place w h dfm1 dx1 dy1 w1 h1 dfm2 dx2 dy2 w2 h2)
  (match-define (deep-flomap argb1-fm z1-fm) dfm1)
  (match-define (deep-flomap argb2-fm z2-fm) dfm2)
  (match-define (flomap argb1-vs 4 argb1-w argb1-h) argb1-fm)
  (match-define (flomap argb2-vs 4 argb2-w argb2-h) argb2-fm)
  (match-define (flomap z1-vs 1 z1-w z1-h) z1-fm)
  (match-define (flomap z2-vs 1 z2-w z2-h) z2-fm)
  
  (define-syntax-rule (get-alpha-pixel vs dx dy w h x y)
    (let ([x  (unsafe-fx- x dx)]
          [y  (unsafe-fx- y dy)])
      (cond [(and (x . unsafe-fx>= . 0) (x . unsafe-fx< . w)
                  (y . unsafe-fx>= . 0) (y . unsafe-fx< . h))
             (unsafe-flvector-ref vs (unsafe-fx* 4 (unsafe-fx+ x (unsafe-fx* y w))))]
            [else  0.0])))
  
  (define-syntax-rule (get-z-pixel vs dx dy w h x y)
    (let ([x  (unsafe-fx- x dx)]
          [y  (unsafe-fx- y dy)])
      (cond [(and (x . unsafe-fx>= . 0) (x . unsafe-fx< . w)
                  (y . unsafe-fx>= . 0) (y . unsafe-fx< . h))
             (unsafe-flvector-ref vs (unsafe-fx+ x (unsafe-fx* y w)))]
            [else  0.0])))
  
  (define z1-max
    (for*/fold ([z1-max -inf.0]) ([y  (in-range h)] [x  (in-range w)])
      (define a1 (get-alpha-pixel argb1-vs dx1 dy1 w1 h1 x y))
      (define a2 (get-alpha-pixel argb2-vs dx2 dy2 w2 h2 x y))
      (cond [(and (a1 . unsafe-fl> . 0.0) (a2 . unsafe-fl> . 0.0))
             (define z1 (get-z-pixel z1-vs dx1 dy1 w1 h1 x y))
             (unsafe-flmax z1-max z1)]
            [else  z1-max])))
  
  (define new-dfm2 (deep-flomap argb2-fm (fm+ z2-fm z1-max)))
  (deep-flomap-superimpose/replace w h dfm1 dx1 dy1 w1 h1 new-dfm2 dx2 dy2 w2 h2 'replace))

(define (deep-flomap-superimpose/blend w h dfm1 dx1 dy1 w1 h1 dfm2 dx2 dy2 w2 h2)
  (match-define (deep-flomap argb1-fm z1-fm) dfm1)
  (match-define (deep-flomap argb2-fm z2-fm) dfm2)
  (define argb1-vs (flomap-values argb1-fm))
  (define argb2-vs (flomap-values argb2-fm))
  (define z1-vs (flomap-values z1-fm))
  (define z2-vs (flomap-values z2-fm))
  
  (define-values (u1-fm v1-fm) (flomap-gradient z1-fm))
  (define-values (u2-fm v2-fm) (flomap-gradient z2-fm))
  (define u1-vs (flomap-values u1-fm))
  (define v1-vs (flomap-values v1-fm))
  (define u2-vs (flomap-values u2-fm))
  (define v2-vs (flomap-values v2-fm))
  
  (define-syntax-rule (get-argbzuv-pixel argb-vs z-vs u-vs v-vs dx dy w h x y)
    (let ([x  (unsafe-fx- x dx)]
          [y  (unsafe-fx- y dy)])
      (cond [(and (x . unsafe-fx>= . 0) (x . unsafe-fx< . w)
                  (y . unsafe-fx>= . 0) (y . unsafe-fx< . h))
             (define i (unsafe-fx+ x (unsafe-fx* y w)))
             (define-values (a r g b) (unsafe-flvector-4ref argb-vs (unsafe-fx* 4 i)))
             (define z (unsafe-flvector-ref z-vs i))
             (define u (unsafe-flvector-ref u-vs i))
             (define v (unsafe-flvector-ref v-vs i))
             (values a r g b z u v)]
            [else
             (values 0.0 0.0 0.0 0.0 0.0 0.0 0.0)])))
  
  (define argb-vs (make-flvector (* 4 w h)))
  (define z-vs (make-flvector (* w h)))
  (for* ([y  (in-range h)] [x  (in-range w)])
    (define-values (a1 r1 g1 b1 z1 u1 v1)
      (get-argbzuv-pixel argb1-vs z1-vs u1-vs v1-vs dx1 dy1 w1 h1 x y))
    (define-values (a2 r2 g2 b2 z2 u2 v2)
      (get-argbzuv-pixel argb2-vs z2-vs u2-vs v2-vs dx2 dy2 w2 h2 x y))
    
    #;; max blending: if both alphas nonzero and unequal, keep the pixel with greatest z
    (define α
      (cond [(and (a1 . unsafe-fl> . 0.0) (a2 . unsafe-fl> . 0.0))
             (cond [(a1 . unsafe-fl> . a2)  0.0]
                   [(a2 . unsafe-fl> . a1)  1.0]
                   [else  (cond [(z1 . unsafe-fl> . z2)  0.0]
                                [(z2 . unsafe-fl> . z1)  1.0]
                                [else  0.5])])]
            [(a1 . unsafe-fl> . 0.0)  0.0]
            [(a2 . unsafe-fl> . 0.0)  1.0]
            [else  0.5]))
    ;; softmax blending
    (define α
      (cond [(and (a1 . unsafe-fl> . 0.0) (a2 . unsafe-fl> . 0.0))
             (define u (unsafe-fl- (unsafe-fl* a2 u2) (unsafe-fl* a1 u1)))
             (define v (unsafe-fl- (unsafe-fl* a2 v2) (unsafe-fl* a1 v1)))
             (define β (unsafe-fl/ (unsafe-fl- (unsafe-fl* a2 z2) (unsafe-fl* a1 z1))
                                   (unsafe-flsqrt (unsafe-fl+ (unsafe-fl* u u) (unsafe-fl* v v)))))
             (unsafe-flsigmoid (unsafe-fl* 15.0 β))]
            [(a1 . unsafe-fl> . 0.0)  0.0]
            [(a2 . unsafe-fl> . 0.0)  1.0]
            [else  0.5]))
    
    (define i (unsafe-fx+ x (unsafe-fx* y w)))
    (unsafe-flvector-4set! argb-vs (unsafe-fx* 4 i)
                           (unsafe-fl-convex-combination a1 a2 α)
                           (unsafe-fl-convex-combination r1 r2 α)
                           (unsafe-fl-convex-combination g1 g2 α)
                           (unsafe-fl-convex-combination b1 b2 α))
    (unsafe-flvector-set! z-vs i (unsafe-fl-convex-combination z1 z2 α)))
  
  (deep-flomap (flomap argb-vs 4 w h)
               (flomap z-vs 1 w h)))

(define (deep-flomap-pin* x1-frac y1-frac x2-frac y2-frac dfm #:z-mode [z-mode 'blend] . dfms)
  (for/fold ([dfm1 dfm]) ([dfm2  (in-list dfms)])
    (define-values (w1 h1) (deep-flomap-size dfm1))
    (define-values (w2 h2) (deep-flomap-size dfm2))
    (deep-flomap-pin dfm1 (* x1-frac w1) (* y1-frac h1)
                     dfm2 (* x2-frac w2) (* y2-frac h2) #:z-mode z-mode)))

(define deep-flomap-superimpose/c (->* [deep-flomap?]
                                       [#:z-mode (one-of/c 'place 'replace 'add 'blend)]
                                       #:rest (listof deep-flomap?)
                                       deep-flomap?))

(define (deep-flomap-lt-superimpose dfm #:z-mode [z-mode 'blend] . dfms)
  (apply deep-flomap-pin* 0 0 0 0 dfm dfms #:z-mode z-mode))

(define (deep-flomap-lc-superimpose dfm #:z-mode [z-mode 'blend] . dfms)
  (apply deep-flomap-pin* 0 1/2 0 1/2 dfm dfms #:z-mode z-mode))

(define (deep-flomap-lb-superimpose dfm #:z-mode [z-mode 'blend] . dfms)
  (apply deep-flomap-pin* 0 1 0 1 dfm dfms #:z-mode z-mode))

(define (deep-flomap-ct-superimpose dfm #:z-mode [z-mode 'blend] . dfms)
  (apply deep-flomap-pin* 1/2 0 1/2 0 dfm dfms #:z-mode z-mode))

(define (deep-flomap-cc-superimpose dfm #:z-mode [z-mode 'blend] . dfms)
  (apply deep-flomap-pin* 1/2 1/2 1/2 1/2 dfm dfms #:z-mode z-mode))

(define (deep-flomap-cb-superimpose dfm #:z-mode [z-mode 'blend] . dfms)
  (apply deep-flomap-pin* 1/2 1 1/2 1 dfm dfms #:z-mode z-mode))

(define (deep-flomap-rt-superimpose dfm #:z-mode [z-mode 'blend] . dfms)
  (apply deep-flomap-pin* 1 0 1 0 dfm dfms #:z-mode z-mode))

(define (deep-flomap-rc-superimpose dfm #:z-mode [z-mode 'blend] . dfms)
  (apply deep-flomap-pin* 1 1/2 1 1/2 dfm dfms #:z-mode z-mode))

(define (deep-flomap-rb-superimpose dfm #:z-mode [z-mode 'blend] . dfms)
  (apply deep-flomap-pin* 1 1 1 1 dfm dfms #:z-mode z-mode))

(define deep-flomap-append/c (->* [deep-flomap?]
                                  #:rest (listof deep-flomap?)
                                  deep-flomap?))

(define (deep-flomap-vl-append dfm . dfms)
  (apply deep-flomap-pin* 0 1 0 0 dfm dfms #:z-mode 'add))

(define (deep-flomap-vc-append dfm . dfms)
  (apply deep-flomap-pin* 1/2 1 1/2 0 dfm dfms #:z-mode 'add))

(define (deep-flomap-vr-append dfm . dfms)
  (apply deep-flomap-pin* 1 1 1 0 dfm dfms #:z-mode 'add))

(define (deep-flomap-ht-append dfm . dfms)
  (apply deep-flomap-pin* 1 0 0 0 dfm dfms #:z-mode 'add))

(define (deep-flomap-hc-append dfm . dfms)
  (apply deep-flomap-pin* 1 1/2 0 1/2 dfm dfms #:z-mode 'add))

(define (deep-flomap-hb-append dfm . dfms)
  (apply deep-flomap-pin* 1 1 0 1 dfm dfms #:z-mode 'add))
