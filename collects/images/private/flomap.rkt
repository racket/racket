#lang racket/base

(require racket/flonum racket/math racket/list racket/match racket/contract racket/class racket/draw
         "unsafe.rkt")

(provide
 (contract-out
  ;; Contracts
  [fx>=/c  (fixnum? . -> . contract?)]
  ;; Data types
  [struct flomap ([values      flvector?]
                  [components  (fx>=/c 0)]
                  [width       (fx>=/c 0)]
                  [height      (fx>=/c 0)])]
  [flomap-size  (flomap? . -> . (values (fx>=/c 0) (fx>=/c 0)))]
  [flomap-ref   (flomap? (fx>=/c 0) fixnum? fixnum? . -> . flonum?)]
  [flomap-bilinear-ref   (flomap? (fx>=/c 0) real? real? . -> . flonum?)]
  ;; Construction and conversion
  [make-flomap       (((fx>=/c 0) (fx>=/c 0) (fx>=/c 0)) (real?) . ->* . flomap?)]
  [build-flomap      ((fx>=/c 0) (fx>=/c 0) (fx>=/c 0)
                                 ((fx>=/c 0) (fx>=/c 0) (fx>=/c 0) . -> . real?)
                                 . -> . flomap?)]
  [make-flomap/components  ((fx>=/c 0) (fx>=/c 0) (listof real?) . -> . flomap?)]
  [flomap-ref-component      (flomap? (fx>=/c 0) . -> . flomap?)]
  [flomap-take-components    (flomap? (fx>=/c 0) . -> . flomap?)]
  [flomap-drop-components    (flomap? (fx>=/c 0) . -> . flomap?)]
  [flomap-append-components  ((flomap?) #:rest (listof flomap?) . ->* . flomap?)]
  [flomap-multiply-alpha  (flomap? . -> . flomap?)]
  [flomap-divide-alpha    (flomap? . -> . flomap?)]
  [bitmap->flomap  ((is-a?/c bitmap%) . -> . flomap?)]
  [flomap->bitmap  (flomap? . -> . (is-a?/c bitmap%))]
  [draw-flomap     ((fx>=/c 0) (fx>=/c 0) ((is-a?/c bitmap-dc%) . -> . any/c) . -> . flomap?)]
  ;; Pointwise unary operations
  [flomap-lift  ((flonum? . -> . real?) . -> . (flomap? . -> . flomap?))]
  [fmneg  (flomap? . -> . flomap?)]
  [fmabs  (flomap? . -> . flomap?)]
  [fmsqr  (flomap? . -> . flomap?)]
  [fmsin  (flomap? . -> . flomap?)]
  [fmcos  (flomap? . -> . flomap?)]
  [fmtan  (flomap? . -> . flomap?)]
  [fmlog  (flomap? . -> . flomap?)]
  [fmexp  (flomap? . -> . flomap?)]
  [fmsqrt  (flomap? . -> . flomap?)]
  [fmasin  (flomap? . -> . flomap?)]
  [fmacos  (flomap? . -> . flomap?)]
  [fmatan  (flomap? . -> . flomap?)]
  [fmround     (flomap? . -> . flomap?)]
  [fmfloor     (flomap? . -> . flomap?)]
  [fmceiling   (flomap? . -> . flomap?)]
  [fmtruncate  (flomap? . -> . flomap?)]
  [flomap-normalize  (flomap? . -> . flomap?)]
  ;; Pointwise binary operations
  [flomap-lift2  (symbol? (flonum? flonum? . -> . real?)
                          . -> . ((or/c flomap? real?) (or/c flomap? real?) . -> . flomap?))]
  [fm+    ((or/c flomap? real?) (or/c flomap? real?) . -> . flomap?)]
  [fm-    ((or/c flomap? real?) (or/c flomap? real?) . -> . flomap?)]
  [fm*    ((or/c flomap? real?) (or/c flomap? real?) . -> . flomap?)]
  [fm/    ((or/c flomap? real?) (or/c flomap? real?) . -> . flomap?)]
  [fmmin  ((or/c flomap? real?) (or/c flomap? real?) . -> . flomap?)]
  [fmmax  ((or/c flomap? real?) (or/c flomap? real?) . -> . flomap?)]
  ;; Blur
  [flomap-gaussian-blur  ((flomap? real?)
                          (real? #:x-stddevs real? #:y-stddevs real?)
                          . ->* . flomap?)]
  [flomap-box-blur  ((flomap? real?) (real?) . ->* . flomap?)]
  [flomap-blur  ((flomap? real?) (real?) . ->* . flomap?)]
  ;[flomap-integral  (flomap? . -> . flomap?)]
  ;; Derivatives
  [flomap-gradient-x       (flomap? . -> . flomap?)]
  [flomap-gradient-y       (flomap? . -> . flomap?)]
  [flomap-gradient         (flomap? . -> . (values flomap? flomap?))]
  [flomap-gradient-normal  (flomap? . -> . flomap?)]
  ;; Statistics
  [flomap-extreme-values  (flomap? . -> . (values flonum? flonum?))]
  [flomap-min-value       (flomap? . -> . flonum?)]
  [flomap-max-value       (flomap? . -> . flonum?)]
  [flomap-nonzero-rect    (flomap? . -> . (values (fx>=/c 0) (fx>=/c 0) (fx>=/c 0)
                                                  (fx>=/c 0) (fx>=/c 0) (fx>=/c 0)))]
  ;; Sizing
  [flomap-inset  (case-> (flomap? fixnum? . -> . flomap?)
                         (flomap? fixnum? fixnum? . -> . flomap?)
                         (flomap? fixnum? fixnum? fixnum? fixnum? . -> . flomap?))]
  [flomap-trim  (flomap? . -> . flomap?)]
  [flomap-crop  (flomap? (fx>=/c 0) (fx>=/c 0) real? real? . -> . flomap?)]
  [flomap-lt-crop  (flomap? (fx>=/c 0) (fx>=/c 0) . -> . flomap?)]
  [flomap-lc-crop  (flomap? (fx>=/c 0) (fx>=/c 0) . -> . flomap?)]
  [flomap-lb-crop  (flomap? (fx>=/c 0) (fx>=/c 0) . -> . flomap?)]
  [flomap-ct-crop  (flomap? (fx>=/c 0) (fx>=/c 0) . -> . flomap?)]
  [flomap-cc-crop  (flomap? (fx>=/c 0) (fx>=/c 0) . -> . flomap?)]
  [flomap-cb-crop  (flomap? (fx>=/c 0) (fx>=/c 0) . -> . flomap?)]
  [flomap-rt-crop  (flomap? (fx>=/c 0) (fx>=/c 0) . -> . flomap?)]
  [flomap-rc-crop  (flomap? (fx>=/c 0) (fx>=/c 0) . -> . flomap?)]
  [flomap-rb-crop  (flomap? (fx>=/c 0) (fx>=/c 0) . -> . flomap?)]
  [flomap-scale   (case-> (flomap? (>=/c 0.0) . -> . flomap?)
                          (flomap? (>=/c 0.0) (>=/c 0.0) . -> . flomap?))]
  [flomap-resize  (flomap? (or/c (fx>=/c 0) #f) (or/c (fx>=/c 0) #f) . -> . flomap?)]
  ;; Transforms
  [flomap-flip-horizontal  (flomap? . -> . flomap?)]
  [flomap-flip-vertical  (flomap? . -> . flomap?)]
  [flomap-transpose  (flomap? . -> . flomap?)]
  [flomap-cw-rotate  (flomap? . -> . flomap?)]
  [flomap-ccw-rotate  (flomap? . -> . flomap?)]
  ;; Compositing
  [flomap-pin   (flomap? real? real? flomap? real? real? . -> . flomap?)]
  [flomap-pin*  ([real? real? real? real? flomap?] #:rest (listof flomap?) . ->* . flomap?)]
  [flomap-lt-superimpose  ([flomap?] #:rest (listof flomap?) . ->* . flomap?)]
  [flomap-lc-superimpose  ([flomap?] #:rest (listof flomap?) . ->* . flomap?)]
  [flomap-lb-superimpose  ([flomap?] #:rest (listof flomap?) . ->* . flomap?)]
  [flomap-ct-superimpose  ([flomap?] #:rest (listof flomap?) . ->* . flomap?)]
  [flomap-cc-superimpose  ([flomap?] #:rest (listof flomap?) . ->* . flomap?)]
  [flomap-cb-superimpose  ([flomap?] #:rest (listof flomap?) . ->* . flomap?)]
  [flomap-rt-superimpose  ([flomap?] #:rest (listof flomap?) . ->* . flomap?)]
  [flomap-rc-superimpose  ([flomap?] #:rest (listof flomap?) . ->* . flomap?)]
  [flomap-rb-superimpose  ([flomap?] #:rest (listof flomap?) . ->* . flomap?)]
  [flomap-vl-append  ([flomap?] #:rest (listof flomap?) . ->* . flomap?)]
  [flomap-vc-append  ([flomap?] #:rest (listof flomap?) . ->* . flomap?)]
  [flomap-vr-append  ([flomap?] #:rest (listof flomap?) . ->* . flomap?)]
  [flomap-ht-append  ([flomap?] #:rest (listof flomap?) . ->* . flomap?)]
  [flomap-hc-append  ([flomap?] #:rest (listof flomap?) . ->* . flomap?)]
  [flomap-hb-append  ([flomap?] #:rest (listof flomap?) . ->* . flomap?)]
  ;; Effects
  [flomap-outline   ([flomap? real?] [#:color (or/c real? (listof real?))] . ->* . flomap?)]
  [flomap-outlined  ([flomap? real?] [#:color (or/c real? (listof real?))] . ->* . flomap?)]
  [flomap-shadow    ([flomap? real?] [#:color (or/c real? (listof real?))] . ->* . flomap?)]
  [flomap-shadowed  ([flomap? real?] [#:color (or/c real? (listof real?))] . ->* . flomap?)]
  )
 unsafe-build-flomap
 flomap-lift/unsafe
 flomap-lift2/unsafe)

(struct flomap (values components width height)
  #:transparent
  #:guard (λ (vs c w h name)
            (unless (= (flvector-length vs) (* c w h))
              (error 'flomap "expected flvector of length ~e; given one of length ~e"
                     (* c w h) (flvector-length vs)))
            (values vs c w h)))

(define (fx>=/c n) (and/c fixnum? (>=/c n)))

(define (flomap-size fm)
  (match-define (flomap _vs _c w h) fm)
  (values w h))

(define-syntax-rule (unsafe-coords->index c w k x y)
  (unsafe-fx+ k (unsafe-fx* c (unsafe-fx+ x (unsafe-fx* y w)))))

(define (flomap-ref* vs c w h k x y)
  (cond [(and (x . unsafe-fx>= . 0) (x . unsafe-fx< . w)
              (y . unsafe-fx>= . 0) (y . unsafe-fx< . h))
         (unsafe-flvector-ref vs (unsafe-coords->index c w k x y))]
        [else  0.0]))

(define (flomap-ref fm k x y)
  (match-define (flomap vs c w h) fm)
  (unless (k . < . c)
    (raise-type-error 'flomap-ref (format "nonnegative fixnum < ~e" c) k))
  (flomap-ref* vs c w h k x y))

(define (flomap-bilinear-ref fm k x y)
  (match-define (flomap vs c w h) fm)
  (unless (k . < . c)
    (raise-type-error 'flomap-bilinear-ref (format "nonnegative fixnum < ~e" c) k))
  (let ([x  (unsafe-fl- (exact->inexact x) 0.5)]
        [y  (unsafe-fl- (exact->inexact y) 0.5)])
    (define floor-x (unsafe-flfloor x))
    (define floor-y (unsafe-flfloor y))
    (define x0 (unsafe-fl->fx floor-x))
    (define y0 (unsafe-fl->fx floor-y))
    (define x1 (unsafe-fx+ x0 1))
    (define y1 (unsafe-fx+ y0 1))
    (define v00 (flomap-ref* vs c w h k x0 y0))
    (define v10 (flomap-ref* vs c w h k x1 y0))
    (define v01 (flomap-ref* vs c w h k x0 y1))
    (define v11 (flomap-ref* vs c w h k x1 y1))
    (define xα (unsafe-fl- x floor-x))
    (unsafe-fl-convex-combination (unsafe-fl-convex-combination v00 v10 xα)
                                  (unsafe-fl-convex-combination v01 v11 xα)
                                  (unsafe-fl- y floor-y))))

;; ===================================================================================================
;; Construction and conversion

(define (make-flomap c w h [v 0.0])
  (flomap (make-flvector (* c w h) (exact->inexact v))
          c w h))

(define (make-flomap/components w h vs)
  (let ([vs  (apply flvector (map exact->inexact vs))])
    (define c (flvector-length vs))
    (define new-vs 
      (for*/flvector #:length (* c w h) ([y  (in-range h)] [x  (in-range w)] [v  (in-flvector vs)])
        v))
    (flomap new-vs c w h)))

(define-syntax-rule (unsafe-build-flomap components width height image-fun)
  (let ([c components] [w width] [h height] [f image-fun])
    (define vs (make-flvector (* c w h)))
    (let y-loop ([y 0] [i 0])
      (cond
        [(y . unsafe-fx< . h)
         (let x-loop ([x 0] [i i])
           (cond
             [(x . unsafe-fx< . w)
              (let k-loop ([k 0] [i i])
                (cond
                  [(k . unsafe-fx< . c)
                   (unsafe-flvector-set! vs i (f k x y))
                   (k-loop (unsafe-fx+ k 1) (unsafe-fx+ i 1))]
                  [else
                   (x-loop (unsafe-fx+ x 1) i)]))]
             [else
              (y-loop (unsafe-fx+ y 1) i)]))]
        [else
         (flomap vs c w h)]))))

(define (build-flomap c w h f)
  (unsafe-build-flomap c w h (λ (k x y) (exact->inexact (f k x y)))))

(define (flomap-ref-component fm k)
  (match-define (flomap vs c w h) fm)
  (unless (k . < . c)
    (error 'flomap-ref-component "expected component index < ~e; given index ~e" c k))
  (unsafe-build-flomap
   1 w h
   (λ (_ x y) (unsafe-flvector-ref vs (unsafe-coords->index c w k x y)))))

(define (flomap-take-components fm c)
  (match-define (flomap vs old-c w h) fm)
  (unless (c . <= . old-c)
    (error 'flomap-take-components "can only take <= ~e components; given ~e" old-c c))
  (unsafe-build-flomap
   c w h
   (λ (k x y) (unsafe-flvector-ref vs (unsafe-coords->index old-c w k x y)))))

(define (flomap-drop-components fm c)
  (match-define (flomap vs old-c w h) fm)
  (unless (c . <= . old-c)
    (error 'flomap-drop-components "can only drop <= ~e components; given ~e" old-c c))
  (unsafe-build-flomap
   (- old-c c) w h
   (λ (k x y) (unsafe-flvector-ref vs (unsafe-coords->index old-c w (unsafe-fx+ k c) x y)))))

(define (flomap-append-components2 fm1 fm2)
  (match-define (flomap vs1 d1 w1 h1) fm1)
  (match-define (flomap vs2 d2 w2 h2) fm2)
  (unless (and (= w1 w2) (= h1 h2))
    (error 'flomap-append-components
           "expected flomaps with equal dimension; given dimensions ~e×~e and ~e×~e"
           w1 h1 w2 h2))
  (unsafe-build-flomap
   (+ d1 d2) w1 h1
   (λ (k x y)
     (cond [(k . unsafe-fx< . d1)  (unsafe-flvector-ref vs1 (unsafe-coords->index d1 w1 k x y))]
           [else  (unsafe-flvector-ref vs2 (unsafe-coords->index d2 w2 (unsafe-fx- k d1) x y))]))))

(define (flomap-append-components fm . fms)
  (for/fold ([fm1 fm]) ([fm2  (in-list fms)])
    (flomap-append-components2 fm1 fm2)))

(define (fldivide x y)
  (if (y . unsafe-fl= . 0.0) 0.0 (unsafe-fl/ x y)))

(define (flomap-divide-alpha fm)
  (match-define (flomap _ c w h) fm)
  (cond [(c . <= . 1)  fm]
        [else
         (define alpha-fm (flomap-ref-component fm 0))
         (flomap-append-components alpha-fm ((flomap-lift2/unsafe 'flomap-divide-alpha fldivide)
                                             (flomap-drop-components fm 1) alpha-fm))]))

(define (flomap-multiply-alpha fm)
  (match-define (flomap _ c w h) fm)
  (cond [(c . > . 1)
         (define alpha-fm (flomap-ref-component fm 0))
         (flomap-append-components alpha-fm (fm* (flomap-drop-components fm 1) alpha-fm))]
        [else  fm]))

(define (bitmap->flomap bm)
  (define w (send bm get-width))
  (define h (send bm get-height))
  (define bs (make-bytes (* 4 w h)))
  ;; get bytes without premultiplying alpha because doing it in flonums maintains precision
  ;; (if RGB bytes are stored without premultiplying alpha)
  (send bm get-argb-pixels 0 0 w h bs #t)
  (send bm get-argb-pixels 0 0 w h bs #f)
  
  (define argb-fm (make-flomap 4 w h))
  (define argb-vs (flomap-values argb-fm))
  (for ([i0  (in-range 0 (* 4 w h) 4)])
    (define i1 (unsafe-fx+ i0 1))
    (define i2 (unsafe-fx+ i0 2))
    (define i3 (unsafe-fx+ i0 3))
    (define a (unsafe-bytes-ref bs i0))
    (define r (unsafe-bytes-ref bs i1))
    (define g (unsafe-bytes-ref bs i2))
    (define b (unsafe-bytes-ref bs i3))
    (unsafe-flvector-set! argb-vs i0 (unsafe-fl/ (unsafe-fx->fl a) 255.0))
    (unsafe-flvector-set! argb-vs i1 (unsafe-fl/ (unsafe-fx->fl r) 255.0))
    (unsafe-flvector-set! argb-vs i2 (unsafe-fl/ (unsafe-fx->fl g) 255.0))
    (unsafe-flvector-set! argb-vs i3 (unsafe-fl/ (unsafe-fx->fl b) 255.0)))
  (flomap-multiply-alpha argb-fm))

(define (flomap->bitmap fm)
  (match-define (flomap vs c w h) fm)
  (let* ([fm  (case c
                [(1)  (flomap-append-components (make-flomap 1 w h 1.0) fm fm fm)]
                [(2)  (define alpha-fm (flomap-ref-component fm 0))
                      (define value-fm (flomap-drop-components fm 1))
                      (flomap-append-components alpha-fm value-fm value-fm value-fm)]
                [(3)  (flomap-append-components (make-flomap 1 w h 1.0) fm)]
                [(4)  fm]
                [else  (raise-type-error 'flomap->bitmap "flomap with 1, 2, 3 or 4 components" fm)])]
         ;; inset if zero (bitmaps can't have zero size)
         [fm  (flomap-inset fm 0 0 (if (= w 0) 1 0) (if (= h 0) 1 0))]
         ;; divide alphas before converting
         [fm  (flomap-divide-alpha fm)])
    ;; guaranteed an ARGB flomap now
    (match-define (flomap vs 4 w h) fm)
    (define bs (make-bytes (* 4 w h)))
    (for ([i0  (in-range 0 (* 4 w h) 4)])
      (define i1 (unsafe-fx+ i0 1))
      (define i2 (unsafe-fx+ i0 2))
      (define i3 (unsafe-fx+ i0 3))
      (define a (unsafe-flvector-ref vs i0))
      (define r (unsafe-flvector-ref vs i1))
      (define g (unsafe-flvector-ref vs i2))
      (define b (unsafe-flvector-ref vs i3))
      (unsafe-bytes-set! bs i0 (unsafe-fl->byte (unsafe-fl* 255.0 a)))
      (unsafe-bytes-set! bs i1 (unsafe-fl->byte (unsafe-fl* 255.0 r)))
      (unsafe-bytes-set! bs i2 (unsafe-fl->byte (unsafe-fl* 255.0 g)))
      (unsafe-bytes-set! bs i3 (unsafe-fl->byte (unsafe-fl* 255.0 b))))
    
    (define bm (make-bitmap w h))
    (send bm set-argb-pixels 0 0 w h bs #t)
    (send bm set-argb-pixels 0 0 w h bs #f)
    bm))

(define (draw-flomap w h draw-proc)
  (define bm (make-bitmap (max w 1) (max h 1)))
  (define dc (make-object bitmap-dc% bm))
  (send dc set-smoothing 'smoothed)
  (draw-proc dc)
  (flomap-inset (bitmap->flomap bm) 0 0 (if (= w 0) -1 0) (if (= h 0) -1 0)))

;; ===================================================================================================
;; Unary pointwise operations

(define-syntax-rule (flomap-lift/unsafe f)
  (λ (fm)
    (match-define (flomap vs c w h) fm)
    (define n (* c w h))
    (define res-vs (make-flvector n))
    (flomap (let loop ([i 0])
              (cond [(i . unsafe-fx< . n)
                     (unsafe-flvector-set! res-vs i (f (unsafe-flvector-ref vs i)))
                     (loop (unsafe-fx+ i 1))]
                    [else  res-vs]))
            c w h)))

(define (flomap-lift op)
  (flomap-lift/unsafe (λ (x) (exact->inexact (op x)))))

(define fmneg (flomap-lift/unsafe unsafe-flneg))
(define fmabs (flomap-lift/unsafe unsafe-flabs))
(define fmsqr (flomap-lift/unsafe (λ (x) (unsafe-fl* x x))))
(define fmsin (flomap-lift/unsafe unsafe-flsin))
(define fmcos (flomap-lift/unsafe unsafe-flcos))
(define fmtan (flomap-lift/unsafe unsafe-fltan))
(define fmlog (flomap-lift/unsafe unsafe-fllog))
(define fmexp (flomap-lift/unsafe unsafe-flexp))
(define fmsqrt (flomap-lift/unsafe unsafe-flsqrt))
(define fmasin (flomap-lift/unsafe unsafe-flasin))
(define fmacos (flomap-lift/unsafe unsafe-flacos))
(define fmatan (flomap-lift/unsafe unsafe-flatan))
(define fmround (flomap-lift/unsafe unsafe-flround))
(define fmfloor (flomap-lift/unsafe unsafe-flfloor))
(define fmceiling (flomap-lift/unsafe unsafe-flceiling))
(define fmtruncate (flomap-lift/unsafe unsafe-fltruncate))

;; ===================================================================================================
;; Binary pointwise operations

(define-syntax-rule (flomap-lift2/unsafe name unsafe-op)
  (λ (fm1 fm2)
    (cond
      [(and (real? fm1) (real? fm2))
       (error name "expected at least one flomap argument; given ~e and ~e" fm1 fm2)]
      [(real? fm1)  (let ([fm1  (exact->inexact fm1)])
                      ((flomap-lift/unsafe (λ (v) (unsafe-op fm1 v))) fm2))]
      [(real? fm2)  (let ([fm2  (exact->inexact fm2)])
                      ((flomap-lift/unsafe (λ (v) (unsafe-op v fm2))) fm1))]
      [else
       (match-define (flomap vs1 c1 w h) fm1)
       (match-define (flomap vs2 c2 w2 h2) fm2)
       (cond
         [(not (and (= w w2) (= h h2)))
          (error name "expected flomaps of equal size; given sizes ~e×~e and ~e×~e" w h w2 h2)]
         [(= c1 c2)
          (define n (* c1 w h))
          (define res-vs (make-flvector n))
          (flomap (let loop ([i 0])
                    (cond [(i . unsafe-fx< . n)
                           (unsafe-flvector-set! res-vs i (unsafe-op (unsafe-flvector-ref vs1 i)
                                                                     (unsafe-flvector-ref vs2 i)))
                           (loop (unsafe-fx+ i 1))]
                          [else  res-vs]))
                  c1 w h)]
         [(= c1 1)
          (unsafe-build-flomap
           c2 w h
           (λ (k x y)
             (unsafe-op (unsafe-flvector-ref vs1 (unsafe-coords->index 1 w 0 x y))
                        (unsafe-flvector-ref vs2 (unsafe-coords->index c2 w k x y)))))]
         [(= c2 1)
          (unsafe-build-flomap
           c1 w h
           (λ (k x y)
             (unsafe-op (unsafe-flvector-ref vs1 (unsafe-coords->index c1 w k x y))
                        (unsafe-flvector-ref vs2 (unsafe-coords->index 1 w 0 x y)))))]
         [else
          (error name (string-append "expected flomaps with the same number of components, "
                                     "or a flomap with 1 component and a flomap with n components; "
                                     "given flomaps with ~e and ~e components")
                 c1 c2)])])))

(define (flomap-lift2 name op)
  (flomap-lift2/unsafe name (λ (x y) (exact->inexact (op x y)))))

(define fm+ (flomap-lift2/unsafe 'fm+ unsafe-fl+))
(define fm- (flomap-lift2/unsafe 'fm- unsafe-fl-))
(define fm* (flomap-lift2/unsafe 'fm* unsafe-fl*))
(define fm/ (flomap-lift2/unsafe 'fm/ unsafe-fl/))
(define fmmin (flomap-lift2/unsafe 'fmmin unsafe-flmin))
(define fmmax (flomap-lift2/unsafe 'fmmax unsafe-flmax))

(define (flomap-normalize fm)
  (match-define (flomap _ _c w h) fm)
  (define-values (v-min v-max) (flomap-extreme-values fm))
  (define v-size (- v-max v-min))
  (let* ([fm  (fm- fm v-min)]
         [fm  (if (zero? v-size) fm (fm/ fm (- v-max v-min)))])
    fm))

;; ===================================================================================================
;; Gaussian blur

(define (flomap-gaussian-blur fm xσ [yσ xσ] #:x-stddevs [x-stddevs 3.0] #:y-stddevs [y-stddevs 3.0])
  (flomap-gaussian-blur-y
   (flomap-gaussian-blur-x fm (abs (exact->inexact xσ)) (abs (exact->inexact x-stddevs)))
   (abs (exact->inexact yσ)) (abs (exact->inexact y-stddevs))))

(define (flomap-gaussian-blur-x fm σ stddevs)
  (let ([σ        (abs (exact->inexact σ))]
        [stddevs  (abs (exact->inexact stddevs))])
    (cond
      [(or (σ . = . 0.0) (stddevs . = . 0.0))  fm]
      [else
       (define dx-min (inexact->exact (floor (* (- stddevs) σ))))
       (define dx-max (+ 1 (inexact->exact (ceiling (* stddevs σ)))))
       (define ss (gaussian-kernel-1d dx-min dx-max σ))
       
       (match-define (flomap vs c w h) fm)
       (unsafe-build-flomap
        c w h
        (λ (k x y)
          (define dx-start (unsafe-fx- (unsafe-fxmax (unsafe-fx+ x dx-min) 0) x))
          (define dx-end (unsafe-fx- (unsafe-fxmin (unsafe-fx+ x dx-max) w) x))
          (define i (unsafe-fx+ k (unsafe-fx* c (unsafe-fx+ x (unsafe-fx* w y)))))
          (define j (unsafe-fx+ i (unsafe-fx* c dx-start)))
          ;; this inner loop has to be *tight*, so no `for'; seems to speed it up by about 50%
          (let src-loop ([sum 0.0] [dx dx-start] [j j])
            (cond [(dx . unsafe-fx< . dx-end)
                   (define s (unsafe-flvector-ref ss (unsafe-fx- dx dx-min)))
                   (src-loop (unsafe-fl+ sum (unsafe-fl* s (unsafe-flvector-ref vs j)))
                             (unsafe-fx+ dx 1)
                             (unsafe-fx+ j c))]
                  [else  sum]))))])))

(define (flomap-gaussian-blur-y fm σ stddevs)
  (let ([σ        (abs (exact->inexact σ))]
        [stddevs  (abs (exact->inexact stddevs))])
    (cond
      [(or (σ . = . 0.0) (stddevs . = . 0.0))  fm]
      [else
       (define dy-min (inexact->exact (floor (* (- stddevs) σ))))
       (define dy-max (+ 1 (inexact->exact (ceiling (* stddevs σ)))))
       (define ss (gaussian-kernel-1d dy-min dy-max σ))
       
       (match-define (flomap vs c w h) fm)
       (define cw (* c w))
       (unsafe-build-flomap
        c w h
        (λ (k x y)
          (define dy-start (unsafe-fx- (unsafe-fxmax (unsafe-fx+ y dy-min) 0) y))
          (define dy-end (unsafe-fx- (unsafe-fxmin (unsafe-fx+ y dy-max) h) y))
          (define i (unsafe-fx+ k (unsafe-fx* c (unsafe-fx+ x (unsafe-fx* w y)))))
          (define j (unsafe-fx+ i (unsafe-fx* cw dy-start)))
          ;; this inner loop has to be *tight*, so no `for'; seems to speed it up by about 50%
          (let src-loop ([sum 0.0] [dy dy-start] [j j])
            (cond [(dy . unsafe-fx< . dy-end)
                   (define s (unsafe-flvector-ref ss (unsafe-fx- dy dy-min)))
                   (src-loop (unsafe-fl+ sum (unsafe-fl* s (unsafe-flvector-ref vs j)))
                             (unsafe-fx+ dy 1)
                             (unsafe-fx+ j cw))]
                  [else  sum]))))])))

(define (gaussian-kernel-1d mn mx σ)
  (define n (- mx mn))
  (define ys
    (for/flvector #:length n ([x  (in-range mn mx)])
      (unsafe-flgaussian (unsafe-fx->fl x) σ)))
  (define s (unsafe-flvector-sum ys))
  (for/flvector #:length n ([y  (in-flvector ys)])
    (unsafe-fl/ y s)))

;; ===================================================================================================
;; Integral images

(define (flomap-integral fm)
  (match-define (flomap vs c w h) fm)
  (define w+1 (+ w 1))
  (define c*w+1 (* c w+1))
  (define h+1 (+ h 1))
  (define new-vs (make-flvector (* c w+1 h+1)))
  (for* ([y  (in-range h)] [x  (in-range w)] [k  (in-range c)])
    (define i (unsafe-coords->index c w k x y))
    (define j00 (unsafe-coords->index c w+1 k x y))
    (define j01 (unsafe-fx+ j00 c*w+1))
    (unsafe-flvector-set! new-vs (unsafe-fx+ j01 c)
                          (unsafe-fl- (unsafe-flsum (unsafe-flvector-ref vs i)
                                                    (unsafe-flvector-ref new-vs j01)
                                                    (unsafe-flvector-ref new-vs (unsafe-fx+ j00 c)))
                                      (unsafe-flvector-ref new-vs j00))))
  (flomap new-vs c w+1 h+1))

(define (unsafe-flomap-integral-sum vs c w h k x-start y-start x-end y-end)
  (define w-1 (unsafe-fx- w 1))
  (define h-1 (unsafe-fx- h 1))
  (define x1 (unsafe-fxmax 0 (unsafe-fxmin x-start w-1)))
  (define x2 (unsafe-fxmax 0 (unsafe-fxmin x-end w-1)))
  (define y1 (unsafe-fxmax 0 (unsafe-fxmin y-start h-1)))
  (define y2 (unsafe-fxmax 0 (unsafe-fxmin y-end h-1)))
  (unsafe-fl- (unsafe-fl+ (unsafe-flvector-ref vs (unsafe-coords->index c w k x1 y1))
                          (unsafe-flvector-ref vs (unsafe-coords->index c w k x2 y2)))
              (unsafe-fl+ (unsafe-flvector-ref vs (unsafe-coords->index c w k x1 y2))
                          (unsafe-flvector-ref vs (unsafe-coords->index c w k x2 y1)))))

(define (flomap-integral-x fm)
  (match-define (flomap vs c w h) fm)
  (define w+1 (+ w 1))
  (define new-vs (make-flvector (* c w+1 h)))
  (for* ([y  (in-range h)] [x  (in-range w)] [k  (in-range c)])
    (define i (unsafe-coords->index c w k x y))
    (define j0 (unsafe-coords->index c w+1 k x y))
    (define j1 (unsafe-fx+ j0 c))
    (unsafe-flvector-set! new-vs j1
                          (unsafe-fl+ (unsafe-flvector-ref vs i)
                                      (unsafe-flvector-ref new-vs j0))))
  (flomap new-vs c w+1 h))

(define (flomap-integral-y fm)
  (match-define (flomap vs c w h) fm)
  (define h+1 (+ h 1))
  (define cw (* c w))
  (define new-vs (make-flvector (* c w h+1)))
  (for* ([y  (in-range h)] [x  (in-range w)] [k  (in-range c)])
    (define j0 (unsafe-coords->index c w k x y))
    (define j1 (unsafe-fx+ j0 cw))
    (unsafe-flvector-set! new-vs j1
                          (unsafe-fl+ (unsafe-flvector-ref vs j0)
                                      (unsafe-flvector-ref new-vs j0))))
  (flomap new-vs c w h+1))

(define (unsafe-flomap-integral-x-sum vs c w k x-start x-end y)
  (define w-1 (unsafe-fx- w 1))
  (define x1 (unsafe-fxmax 0 (unsafe-fxmin x-start w-1)))
  (define x2 (unsafe-fxmax 0 (unsafe-fxmin x-end w-1)))
  (unsafe-fl- (unsafe-flvector-ref vs (unsafe-coords->index c w k x2 y))
              (unsafe-flvector-ref vs (unsafe-coords->index c w k x1 y))))

(define (unsafe-flomap-integral-y-sum vs c w h k x y-start y-end)
  (define h-1 (unsafe-fx- h 1))
  (define y1 (unsafe-fxmax 0 (unsafe-fxmin y-start h-1)))
  (define y2 (unsafe-fxmax 0 (unsafe-fxmin y-end h-1)))
  (unsafe-fl- (unsafe-flvector-ref vs (unsafe-coords->index c w k x y2))
              (unsafe-flvector-ref vs (unsafe-coords->index c w k x y1))))

;; ===================================================================================================
;; Box blur

(define (flomap-box-blur fm xr [yr xr])
  (let ([xr  (abs xr)] [yr  (abs yr)])
    (cond [(and (integer? xr) (integer? yr))
           (flomap-box-blur/int fm (inexact->exact xr) (inexact->exact yr))]
          [else
           (flomap-box-blur-y (flomap-box-blur-x fm xr) yr)])))

(define (flomap-box-blur-x fm r)
  (cond
    [(integer? r)  (flomap-box-blur-x/int fm (inexact->exact r))]
    [else
     (define r1 (inexact->exact (floor r)))
     (define r2 (+ r1 1))
     (define s (+ 1 (* 2 r)))
     (define s1 (+ 1 (* 2 r1)))
     (define s2 (+ 1 (* 2 r2)))
     (define α (exact->inexact (/ (- (sqr s2) (sqr s)) (- (sqr s2) (sqr s1)))))
     (define norm1 (/ α s1))
     (define norm2 (/ (- 1 α) s2))
     (define r1+1 (+ r1 1))
     (define r2+1 (+ r2 1))
     (match-define (flomap _ c w h) fm)
     (match-define (flomap int-vs int-c int-w int-h) (flomap-integral-x fm))
     (unsafe-build-flomap
      c w h
      (λ (k x y)
        (unsafe-fl+
         (unsafe-fl* norm1 (unsafe-flomap-integral-x-sum
                            int-vs int-c int-w k
                            (unsafe-fx- x r1) (unsafe-fx+ x r1+1) y))
         (unsafe-fl* norm2 (unsafe-flomap-integral-x-sum
                            int-vs int-c int-w k
                            (unsafe-fx- x r2) (unsafe-fx+ x r2+1) y)))))]))

(define (flomap-box-blur-y fm r)
  (cond
    [(integer? r)  (flomap-box-blur-y/int fm (inexact->exact r))]
    [else
     (define r1 (inexact->exact (floor r)))
     (define r2 (+ r1 1))
     (define s (+ 1 (* 2 r)))
     (define s1 (+ 1 (* 2 r1)))
     (define s2 (+ 1 (* 2 r2)))
     (define α (exact->inexact (/ (- (sqr s2) (sqr s)) (- (sqr s2) (sqr s1)))))
     (define norm1 (/ α s1))
     (define norm2 (/ (- 1 α) s2))
     (define r1+1 (+ r1 1))
     (define r2+1 (+ r2 1))
     (match-define (flomap _ c w h) fm)
     (match-define (flomap int-vs int-c int-w int-h) (flomap-integral-y fm))
     (unsafe-build-flomap
      c w h
      (λ (k x y)
        (unsafe-fl+
         (unsafe-fl* norm1 (unsafe-flomap-integral-y-sum
                            int-vs int-c int-w int-h k x
                            (unsafe-fx- y r1) (unsafe-fx+ y r1+1)))
         (unsafe-fl* norm2 (unsafe-flomap-integral-y-sum
                            int-vs int-c int-w int-h k x
                            (unsafe-fx- y r2) (unsafe-fx+ y r2+1))))))]))

(define (flomap-box-blur/int fm xr yr)
  (define norm (/ 1.0 (* (+ 1 (* 2 xr)) (+ 1 (* 2 yr)))))
  (define xr+1 (+ xr 1))
  (define yr+1 (+ yr 1))
  (match-define (flomap _ c w h) fm)
  (match-define (flomap int-vs int-c int-w int-h) (flomap-integral fm))
  (unsafe-build-flomap
   c w h
   (λ (k x y)
     (unsafe-fl* norm (unsafe-flomap-integral-sum
                       int-vs int-c int-w int-h k
                       (unsafe-fx- x xr) (unsafe-fx- y yr)
                       (unsafe-fx+ x xr+1) (unsafe-fx+ y yr+1))))))

(define (flomap-box-blur-x/int fm r)
  (define norm (/ 1.0 (+ 1 (* 2 r))))
  (define r+1 (+ r 1))
  (match-define (flomap _ c w h) fm)
  (match-define (flomap int-vs int-c int-w int-h) (flomap-integral-x fm))
  (unsafe-build-flomap
   c w h
   (λ (k x y)
     (unsafe-fl* norm (unsafe-flomap-integral-x-sum
                       int-vs int-c int-w k
                       (unsafe-fx- x r) (unsafe-fx+ x r+1) y)))))

(define (flomap-box-blur-y/int fm r)
  (define norm (/ 1.0 (+ 1 (* 2 r))))
  (define r+1 (+ r 1))
  (match-define (flomap _ c w h) fm)
  (match-define (flomap int-vs int-c int-w int-h) (flomap-integral-y fm))
  (unsafe-build-flomap
   c w h
   (λ (k x y)
     (unsafe-fl* norm (unsafe-flomap-integral-y-sum
                       int-vs int-c int-w int-h k x
                       (unsafe-fx- y r) (unsafe-fx+ y r+1))))))

;; ===================================================================================================
;; Default blur

(define (flomap-blur fm xσ [yσ xσ])
  (let ([xσ  (abs xσ)] [yσ  (abs yσ)])
    (cond
      [(and (xσ . >= . 1.5) (yσ . >= . 1.5))
       (define xσ^2 (sqr xσ))
       (define yσ^2 (sqr yσ))
       (define xr (floor (variance->box-radius (* 1/3 xσ^2))))
       (define yr (floor (variance->box-radius (* 1/3 yσ^2))))
       (flomap-box-blur (flomap-box-blur (flomap-box-blur fm xr yr) xr yr)
                        (variance->box-radius (- xσ^2 (* 2 (box-radius->variance xr))))
                        (variance->box-radius (- yσ^2 (* 2 (box-radius->variance yr)))))]
      [else
       (flomap-blur-x (flomap-blur-y fm yσ) xσ)])))

(define (box-radius->variance r)
  (* 1/12 (sqr (+ 1 (* 2 r)))))

(define (variance->box-radius σ^2)
  (* 1/2 (- (sqrt (* 12 σ^2)) 1)))

(define ((make-flomap-blur-dimension gaussian-blur box-blur) fm σ)
  (cond
    [(σ . = . 0.0)  fm]
    [(σ . < . 1.5)  (gaussian-blur fm σ 3.0)]
    [else
     (define σ^2 (sqr σ))
     (define r (floor (variance->box-radius (* 1/3 σ^2))))
     (box-blur (box-blur (box-blur fm r) r)
               (variance->box-radius (- σ^2 (* 2 (box-radius->variance r)))))]))

(define flomap-blur-x (make-flomap-blur-dimension flomap-gaussian-blur-x flomap-box-blur-x))
(define flomap-blur-y (make-flomap-blur-dimension flomap-gaussian-blur-y flomap-box-blur-y))

;; ===================================================================================================
;; Derivatives (Schurr operator)

(define (flomap-gradient-x fm)
  (match-define (flomap vs c w h) fm)
  (define cw (* c w))
  (define d00 (+ (- cw) -1))
  (define d20 (+ (- cw) 1))
  (define d02 (+ cw -1))
  (define d22 (+ cw 1))
  (define w-1 (- w 1))
  (define h-1 (- h 1))
  (unsafe-build-flomap
   c w h
   (λ (k x y)
     (cond [(and (x . unsafe-fx> . 0) (x . unsafe-fx< . w-1)
                 (y . unsafe-fx> . 0) (y . unsafe-fx< . h-1))
            (define i (unsafe-fx+ k (unsafe-fx* c (unsafe-fx+ x (unsafe-fx* w y)))))
            (unsafe-flsum
             (unsafe-fl- (unsafe-fl* 0.1875 (unsafe-flvector-ref vs (unsafe-fx+ i d20)))
                         (unsafe-fl* 0.1875 (unsafe-flvector-ref vs (unsafe-fx+ i d00))))
             (unsafe-fl- (unsafe-fl* 0.6250 (unsafe-flvector-ref vs (unsafe-fx+ i 1)))
                         (unsafe-fl* 0.6250 (unsafe-flvector-ref vs (unsafe-fx- i 1))))
             (unsafe-fl- (unsafe-fl* 0.1875 (unsafe-flvector-ref vs (unsafe-fx+ i d22)))
                         (unsafe-fl* 0.1875 (unsafe-flvector-ref vs (unsafe-fx+ i d02)))))]
           [else  0.0]))))

(define (flomap-gradient-y fm)
  (match-define (flomap vs c w h) fm)
  (define cw (* c w))
  (define d00 (+ (- cw) -1))
  (define d02 (+ cw -1))
  (define d20 (+ (- cw) 1))
  (define d22 (+ cw 1))
  (define w-1 (- w 1))
  (define h-1 (- h 1))
  (unsafe-build-flomap
   c w h
   (λ (k x y)
     (cond [(and (x . unsafe-fx> . 0) (x . unsafe-fx< . w-1)
                 (y . unsafe-fx> . 0) (y . unsafe-fx< . h-1))
            (define i (unsafe-fx+ k (unsafe-fx* c (unsafe-fx+ x (unsafe-fx* w y)))))
            (unsafe-flsum
             (unsafe-fl- (unsafe-fl* 0.1875 (unsafe-flvector-ref vs (unsafe-fx+ i d02)))
                         (unsafe-fl* 0.1875 (unsafe-flvector-ref vs (unsafe-fx+ i d00))))
             (unsafe-fl- (unsafe-fl* 0.6250 (unsafe-flvector-ref vs (unsafe-fx+ i cw)))
                         (unsafe-fl* 0.6250 (unsafe-flvector-ref vs (unsafe-fx- i cw))))
             (unsafe-fl- (unsafe-fl* 0.1875 (unsafe-flvector-ref vs (unsafe-fx+ i d22)))
                         (unsafe-fl* 0.1875 (unsafe-flvector-ref vs (unsafe-fx+ i d20)))))]
           [else  0.0]))))

(define (flomap-gradient fm)
  (values (flomap-gradient-x fm) (flomap-gradient-y fm)))

(define (flomap-gradient-normal z-fm)
  (define-values (dx-bm dy-bm) (flomap-gradient z-fm))
  (match-define (flomap dxs 1 w h) dx-bm)
  (match-define (flomap dys 1 _w _h) dy-bm)
  (define normal-vs (make-flvector (* 3 w h)))
  (for ([i  (in-range (* w h))])
    (define j (unsafe-fx* 3 i))
    (define dx (unsafe-flvector-ref dxs i))
    (define dy (unsafe-flvector-ref dys i))
    (define-values (nx ny nz) (unsafe-fl3normalize (unsafe-flneg dx) (unsafe-flneg dy) 2.0))
    (unsafe-flvector-3set! normal-vs j nx ny nz))
  (flomap normal-vs 3 w h))

;; ===================================================================================================
;; Statistics

(define (flomap-min-value fm)
  (define vs (flomap-values fm))
  (for/fold ([v-min 0.0]) ([v  (in-flvector vs)])
    (unsafe-flmin v-min v)))

(define (flomap-max-value fm)
  (define vs (flomap-values fm))
  (for/fold ([v-max 0.0]) ([v  (in-flvector vs)])
    (unsafe-flmax v-max v)))

(define (flomap-extreme-values fm)
  (define vs (flomap-values fm))
  (for/fold ([v-min 0.0] [v-max 0.0]) ([v  (in-flvector vs)])
    (values (unsafe-flmin v-min v)
            (unsafe-flmax v-max v))))

(define (flomap-nonzero-rect fm)
  (match-define (flomap vs c w h) fm)
  (for*/fold ([k-min c] [x-min w] [y-min h] [k-max 0] [x-max 0] [y-max 0]
                        ) ([y  (in-range h)] [x  (in-range w)] [k  (in-range c)])
    (define i (unsafe-fx+ k (unsafe-fx* c (unsafe-fx+ x (unsafe-fx* y w)))))
    (define v (unsafe-flvector-ref vs i))
    (cond [(not (v . unsafe-fl= . 0.0))
           (values (unsafe-fxmin k-min k)
                   (unsafe-fxmin x-min x)
                   (unsafe-fxmin y-min y)
                   (unsafe-fxmax k-max (unsafe-fx+ 1 k))
                   (unsafe-fxmax x-max (unsafe-fx+ 1 x))
                   (unsafe-fxmax y-max (unsafe-fx+ 1 y)))]
          [else  (values k-min x-min y-min k-max x-max y-max)])))

;; ===================================================================================================
;; Sizing

(define flomap-inset
  (case-lambda
    [(fm amt)
     (flomap-inset fm amt amt amt amt)]
    [(fm h-amt v-amt)
     (flomap-inset fm h-amt v-amt h-amt v-amt)]
    [(fm l-amt t-amt r-amt b-amt)
     (cond [(and (= l-amt 0) (= t-amt 0) (= r-amt 0) (= b-amt 0))  fm]
           [else
            (match-define (flomap vs c w h) fm)
            (define new-w (+ w l-amt r-amt))
            (define new-h (+ h t-amt b-amt))
            (define new-vs (make-flvector (* c new-w new-h)))
            (for ([new-y  (in-range new-h)])
              (define y (- new-y t-amt))
              (when (and (y . >= . 0) (y . < . h))
                (for ([new-x  (in-range new-w)])
                  (define x (- new-x l-amt))
                  (when (and (x . >= . 0) (x . < . w))
                    (for ([k  (in-range c)])
                      (define i (unsafe-coords->index c w k x y))
                      (define new-i (unsafe-coords->index c new-w k new-x new-y))
                      (unsafe-flvector-set! new-vs new-i (unsafe-flvector-ref vs i)))))))
            (flomap new-vs c new-w new-h)])]))

(define (flomap-trim fm)
  (match-define (flomap _ c w h) fm)
  (unless (c . > . 0)
    (raise-type-error 'flomap-shadow "flomap with at least 1 component" fm))
  (define-values (_k-min x-min y-min _k-max x-max y-max)
    (flomap-nonzero-rect (flomap-ref-component fm 0)))
  (flomap-inset fm (- x-min) (- y-min) (- x-max w) (- y-max h)))

(define (flomap-crop fm width height x-frac y-frac)
  (match-define (flomap _ c w h) fm)
  (define l-amt (round (* x-frac (- width w))))
  (define r-amt (- (- width w) l-amt))
  (define t-amt (round (* y-frac (- height h))))
  (define b-amt (- (- height h) t-amt))
  (flomap-inset fm l-amt t-amt r-amt b-amt))

(define (flomap-lt-crop fm w h) (flomap-crop fm w h 0 0))
(define (flomap-lc-crop fm w h) (flomap-crop fm w h 0 1/2))
(define (flomap-lb-crop fm w h) (flomap-crop fm w h 0 1))
(define (flomap-ct-crop fm w h) (flomap-crop fm w h 1/2 0))
(define (flomap-cc-crop fm w h) (flomap-crop fm w h 1/2 1/2))
(define (flomap-cb-crop fm w h) (flomap-crop fm w h 1/2 1))
(define (flomap-rt-crop fm w h) (flomap-crop fm w h 1 0))
(define (flomap-rc-crop fm w h) (flomap-crop fm w h 1 1/2))
(define (flomap-rb-crop fm w h) (flomap-crop fm w h 1 1))

(define flomap-scale
  (case-lambda
    [(fm scale)            (flomap-scale fm scale scale)]
    [(fm x-scale y-scale)  (flomap-scale-x (flomap-scale-y fm (exact->inexact y-scale))
                                           (exact->inexact x-scale))]))

(define (flomap-resize fm width height)
  (cond [(and width height)  (flomap-resize-x (flomap-resize-y fm height) width)]
        [width   (define s (exact->inexact (/ width (flomap-width fm))))
                 (flomap-resize-x (flomap-scale-y fm s) width)]
        [height  (define s (exact->inexact (/ height (flomap-height fm))))
                 (flomap-scale-x (flomap-resize-y fm height) s)]))

(define (flomap-scale-x fm scale)
  (cond [(= 0 scale)  (match-define (flomap _ c w h) fm)
                      (make-flomap c 0 h)]
        [else  (flomap-scale*-x fm scale (inexact->exact (ceiling (* (flomap-width fm) scale))))]))

(define (flomap-scale-y fm scale)
  (cond [(= 0 scale)  (match-define (flomap _ c w h) fm)
                      (make-flomap c w 0)]
        [else  (flomap-scale*-y fm scale (inexact->exact (ceiling (* (flomap-height fm) scale))))]))

(define (flomap-resize-x fm width)
  (cond [(= 0 width)  (match-define (flomap _ c w h) fm)
                      (make-flomap c 0 h)]
        [else  (flomap-scale*-x fm (exact->inexact (/ width (flomap-width fm))) width)]))

(define (flomap-resize-y fm height)
  (cond [(= 0 height)  (match-define (flomap _ c w h) fm)
                       (make-flomap c w 0)]
        [else  (flomap-scale*-y fm (exact->inexact (/ height (flomap-height fm))) height)]))

;; standard deviation of an unscaled box filter (i.e. f([-1/2,1/2]) = {1}, zero elsewhere)
(define box-filter-variance 1/12)
;; standard deviation of an unscaled triangle filter (simualtes effect of linear interpolation)
(define triangle-filter-variance 1/24)

;; calculates the standard deviation of downscaling blur, assuming linear interpolation will be
;; carried out on the blurred image
(define (stddev-for-scale scale)
  (define var (- (/ box-filter-variance (sqr scale))
                 triangle-filter-variance))
  (sqrt (max 0 var)))

(define (flomap-scale*-x fm scale width)
  (cond [(scale . = . 1.0)  fm]
        [(scale . > . 1.0)  (flomap-scale*-x/linear fm scale width)]
        [else  (define low-res-fm
                 (flomap-gaussian-blur-x fm (stddev-for-scale scale) 2.0))
               (flomap-scale*-x/linear low-res-fm scale width)]))

(define (flomap-scale*-y fm scale height)
  (cond [(scale . = . 1.0)  fm]
        [(scale . > . 1.0)  (flomap-scale*-y/linear fm scale height)]
        [else  (define low-res-fm
                 (flomap-gaussian-blur-y fm (stddev-for-scale scale) 2.0))
               (flomap-scale*-y/linear low-res-fm scale height)]))

(define (flomap-scale*-x/linear fm s new-w)
  (match-define (flomap vs c w h) fm)
  (define w-1 (unsafe-fx- w 1))
  (unsafe-build-flomap
   c new-w h
   (λ (k new-x y)
     (define scaled-x (unsafe-fl- (unsafe-fl/ (unsafe-fl+ (unsafe-fx->fl new-x) 0.5) s) 0.5))
     (define floor-scaled-x (unsafe-flfloor scaled-x))
     (define x0 (unsafe-fl->fx floor-scaled-x))
     (cond [(or (x0 . unsafe-fx< . 0) (x0 . unsafe-fx>= . w)) 0.0]
           [else
            (define i0 (unsafe-coords->index c w k x0 y))
            (define v0 (unsafe-flvector-ref vs i0))
            (define v1 (cond [(x0 . unsafe-fx= . w-1)  0.0]
                             [else  (unsafe-flvector-ref vs (unsafe-fx+ i0 c))]))
            (unsafe-fl-convex-combination v0 v1 (unsafe-fl- scaled-x  floor-scaled-x))]))))

(define (flomap-scale*-y/linear fm s new-h)
  (match-define (flomap vs c w h) fm)
  (define h-1 (unsafe-fx- h 1))
  (define cw (* c w))
  (unsafe-build-flomap
   c w new-h
   (λ (k x new-y)
     (define orig-y (unsafe-fl+ (unsafe-fx->fl new-y) 0.5))
     (define scaled-y (unsafe-fl/ orig-y s))
     (define half-floor-y (unsafe-fl- (unsafe-flfloor (unsafe-fl+ scaled-y 0.5)) 0.5))
     (define y0 (unsafe-fl->fx (unsafe-flfloor half-floor-y)))
     (cond [(or (y0 . unsafe-fx< . 0) (y0 . unsafe-fx>= . h))  0.0]
           [else
            (define i0 (unsafe-coords->index c w k x y0))
            (define v0 (unsafe-flvector-ref vs i0))
            (define v1 (cond [(y0 . unsafe-fx= . h-1)  0.0]
                             [else  (unsafe-flvector-ref vs (unsafe-fx+ i0 cw))]))
            (unsafe-fl-convex-combination v0 v1 (unsafe-fl- scaled-y half-floor-y))]))))

;; ===================================================================================================
;; Pinning and standard pin-derived combiners

(define (flomap-pin fm1 x1 y1 fm2 x2 y2)
  (cond
    [(not (and (zero? x2) (zero? y2)))
     (flomap-pin fm1 (- x1 x2) (- y1 y2) fm2 0 0)]
    [else
     (match-define (flomap argb1-vs 4 w1 h1) fm1)
     (match-define (flomap argb2-vs 4 w2 h2) fm2)
     
     ;; fm1 and fm2 offsets, in final image coordinates
     (define dx1 (inexact->exact (round (max 0 (- x1)))))
     (define dy1 (inexact->exact (round (max 0 (- y1)))))
     (define dx2 (inexact->exact (round (max 0 x1))))
     (define dy2 (inexact->exact (round (max 0 y1))))
     
     ;; final image size
     (define w (max (+ dx1 w1) (+ dx2 w2)))
     (define h (max (+ dy1 h1) (+ dy2 h2)))
     
     (define-syntax-rule (get-argb-pixel argb-vs dx dy w h x y)
       (let ([x  (unsafe-fx- x dx)]
             [y  (unsafe-fx- y dy)])
         (cond [(and (x . unsafe-fx>= . 0) (x . unsafe-fx< . w)
                     (y . unsafe-fx>= . 0) (y . unsafe-fx< . h))
                (unsafe-flvector-4ref argb-vs (unsafe-coords->index 4 w 0 x y))]
               [else
                (values 0.0 0.0 0.0 0.0)])))
     
     (define argb-vs (make-flvector (* 4 w h)))
     (for* ([y  (in-range h)] [x  (in-range w)])
       (define-values (a1 r1 g1 b1) (get-argb-pixel argb1-vs dx1 dy1 w1 h1 x y))
       (define-values (a2 r2 g2 b2) (get-argb-pixel argb2-vs dx2 dy2 w2 h2 x y))
       (unsafe-flvector-4set! argb-vs (unsafe-coords->index 4 w 0 x y)
                              (unsafe-fl-alpha-blend a1 a2 a2)
                              (unsafe-fl-alpha-blend r1 r2 a2)
                              (unsafe-fl-alpha-blend g1 g2 a2)
                              (unsafe-fl-alpha-blend b1 b2 a2)))
     
     (flomap argb-vs 4 w h)]))

(define (flomap-pin* x1-frac y1-frac x2-frac y2-frac fm . fms)
  (for/fold ([fm1 fm]) ([fm2  (in-list fms)])
    (define-values (w1 h1) (flomap-size fm1))
    (define-values (w2 h2) (flomap-size fm2))
    (flomap-pin fm1 (* x1-frac w1) (* y1-frac h1)
                fm2 (* x2-frac w2) (* y2-frac h2))))

(define (flomap-lt-superimpose fm . fms)
  (apply flomap-pin* 0 0 0 0 fm fms))

(define (flomap-lc-superimpose fm . fms)
  (apply flomap-pin* 0 1/2 0 1/2 fm fms))

(define (flomap-lb-superimpose fm . fms)
  (apply flomap-pin* 0 1 0 1 fm fms))

(define (flomap-ct-superimpose fm . fms)
  (apply flomap-pin* 1/2 0 1/2 0 fm fms))

(define (flomap-cc-superimpose fm . fms)
  (apply flomap-pin* 1/2 1/2 1/2 1/2 fm fms))

(define (flomap-cb-superimpose fm . fms)
  (apply flomap-pin* 1/2 1 1/2 1 fm fms))

(define (flomap-rt-superimpose fm . fms)
  (apply flomap-pin* 1 0 1 0 fm fms))

(define (flomap-rc-superimpose fm . fms)
  (apply flomap-pin* 1 1/2 1 1/2 fm fms))

(define (flomap-rb-superimpose fm . fms)
  (apply flomap-pin* 1 1 1 1 fm fms))

(define (flomap-vl-append fm . fms)
  (apply flomap-pin* 0 1 0 0 fm fms))

(define (flomap-vc-append fm . fms)
  (apply flomap-pin* 1/2 1 1/2 0 fm fms))

(define (flomap-vr-append fm . fms)
  (apply flomap-pin* 1 1 1 0 fm fms))

(define (flomap-ht-append fm . fms)
  (apply flomap-pin* 1 0 0 0 fm fms))

(define (flomap-hc-append fm . fms)
  (apply flomap-pin* 1 1/2 0 1/2 fm fms))

(define (flomap-hb-append fm . fms)
  (apply flomap-pin* 1 1 0 1 fm fms))

;; ===================================================================================================
;; Transforms

(define (flomap-flip-horizontal fm)
  (match-define (flomap vs c w h) fm)
  (define w-1 (- w 1))
  (unsafe-build-flomap
   c w h
   (λ (k x y)
     (unsafe-flvector-ref vs (unsafe-coords->index c w k (unsafe-fx- w-1 x) y)))))

(define (flomap-flip-vertical fm)
  (match-define (flomap vs c w h) fm)
  (define h-1 (- h 1))
  (unsafe-build-flomap
   c w h
   (λ (k x y)
     (unsafe-flvector-ref vs (unsafe-coords->index c w k x (unsafe-fx- h-1 y))))))

(define (flomap-transpose fm)
  (match-define (flomap vs c w h) fm)
  (unsafe-build-flomap
   c h w
   (λ (k x y)
     (unsafe-flvector-ref vs (unsafe-coords->index c w k y x)))))

(define (flomap-cw-rotate fm)
  (match-define (flomap vs c w h) fm)
  (define h-1 (- h 1))
  (unsafe-build-flomap
   c h w
   (λ (k x y)
     (unsafe-flvector-ref vs (unsafe-coords->index c w k (unsafe-fx- h-1 y) x)))))

(define (flomap-ccw-rotate fm)
  (match-define (flomap vs c w h) fm)
  (define w-1 (- w 1))
  (unsafe-build-flomap
   c h w
   (λ (k x y)
     (unsafe-flvector-ref vs (unsafe-coords->index c w k y (unsafe-fx- w-1 x))))))


;; ===================================================================================================
;; Effects

(define (colorize-alpha fm color)
  (match-define (flomap _ 1 w h) fm)
  (flomap-append-components fm (fm* fm (make-flomap/components w h color))))

(define (flomap-outline fm amt #:color [color #f])
  (match-define (flomap _ c w h) fm)
  (define σ (* 0.5 (max 1.0 amt)))
  (define ceiling-amt (inexact->exact (ceiling amt)))
  (define test-size (* 2 (+ 1 ceiling-amt)))
  (define test-mid (quotient test-size 2))
  (define test-fm (build-flomap 1 test-size test-size
                                (λ (k x y) (if (x . >= . test-mid) 1.0 0.0))))
  (define blur-fm (flomap-blur test-fm σ))
  (define v-max (flomap-bilinear-ref blur-fm 0 (+ 0.5 (- test-mid amt)) test-mid))
  (define v-min (flomap-bilinear-ref blur-fm 0 (+ 0.5 (- test-mid amt 1)) test-mid))
  (define alpha-fm (flomap-ref-component fm 0))
  (define new-alpha-fm (fmmax 0.0 (fmmin 1.0 (fm/ (fm- (flomap-blur alpha-fm σ) v-min)
                                                  (- v-max v-min)))))
  (define color-vs (if (list? color) color (make-list (- c 1) 0.0)))
  (colorize-alpha new-alpha-fm color-vs))

(define (flomap-outlined fm amt #:color [color #f])
  (flomap-cc-superimpose (flomap-outline fm amt #:color color) fm))

(define (flomap-shadow fm σ #:color [color #f])
  (match-define (flomap _ c w h) fm)
  (cond [(c . = . 0)  fm]
        [else  (define alpha-fm (flomap-ref-component fm 0))
               (define color-vs (if (list? color) color (make-list (- c 1) 0.0)))
               (colorize-alpha (flomap-blur alpha-fm σ) color-vs)]))

(define (flomap-shadowed fm σ #:color [color #f])
  (flomap-cc-superimpose (flomap-shadow fm σ #:color color) fm))
