#lang racket/base

;; Extra drawing, font, color and style functions.

(require racket/draw racket/class racket/match racket/list racket/contract racket/math racket/flonum
         unstable/latent-contract/defthing
         "math.rkt"
         "utils.rkt"
         "contract.rkt"
         "sample.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Drawing text rotated around an anchor point

(define sin45 (/ 1 (sqrt 2)))

(define (draw-text/anchor dc str x y [anchor 'top-left] [angle 0] [dist 0])
  (define-values (width height _1 _2) (send dc get-text-extent str #f #t 0))
  (let ([dist  (case anchor
                 [(top-left bottom-left top-right bottom-right)  (* sin45 dist)]
                 [else  dist])])
    (define dx (case anchor
                 [(top-left left bottom-left)     (- dist)]
                 [(top center bottom)             (* 1/2 width)]
                 [(top-right right bottom-right)  (+ width dist)]
                 [else  (raise-type-error 'draw-text/anchor "anchor/c" anchor)]))
    (define dy (case anchor
                 [(top-left top top-right)           (- dist)]
                 [(left center right)                (* 1/2 height)]
                 [(bottom-left bottom bottom-right)  (+ height dist)]))
    (define rdx (+ (* (sin angle) dy) (* (cos angle) dx)))
    (define rdy (- (* (cos angle) dy) (* (sin angle) dx)))
    
    (send dc draw-text str (- x rdx) (- y rdy) #t 0 angle)))

(define (get-text-corners/anchor dc str x y [anchor 'top-left] [angle 0] [dist 0])
  (define-values (width height _1 _2) (send dc get-text-extent str #f #t 0))
  (let ([dist  (case anchor
                 [(top-left bottom-left top-right bottom-right)  (* sin45 dist)]
                 [else  dist])])
    (define dxs (case anchor
                  [(top-left left bottom-left)     (list (- dist) (- width dist))]
                  [(top center bottom)             (list (* -1/2 width) (* 1/2 width))]
                  [(top-right right bottom-right)  (list (- dist width) dist)]
                  [else  (raise-type-error 'get-text-corners/anchor "anchor/c" anchor)]))
    (define dys (case anchor
                  [(top-left top top-right)           (list (- dist) (- height dist))]
                  [(left center right)                (list (* -1/2 height) (* 1/2 width))]
                  [(bottom-left bottom bottom-right)  (list (- dist height) dist)]))
    
    (for*/list ([dx  (in-list dxs)] [dy  (in-list dys)])
      (define rdx (+ (* (sin angle) dy) (* (cos angle) dx)))
      (define rdy (- (* (cos angle) dy) (* (sin angle) dx)))
      (vector (+ x rdx) (+ y rdy)))))

;; ===================================================================================================
;; Draw paramter normalization

(define (real->font-size size)
  (define i (inexact->exact (round size)))
  (min (max i 1) 255))

(define (real->color-byte f)
  (cond [(rational? f)  (define i (inexact->exact (floor f)))
                        (min (max i 0) 255)]
        [(eqv? f +inf.0)  255]
        [else  0]))

;; Returns an immutable instance of color%. Immutable colors are faster because they don't have to
;; have immutable copies made when they're used in a dc.
(define (make-color% r g b)
  (make-color r g b))

;; Returns an immutable instance of pen%. Same reasoning as for make-color%.
(define (make-pen% r g b w s)
  (make-pen #:color (make-color% r g b) #:width w #:style s))

;; Returns an immutable instance of brush%. Same reasoning as for make-color%.
(define (make-brush% r g b s)
  (make-brush #:color (make-color% r g b) #:style s))

(define (color%? c) (is-a? c color%))

(defproc (->color [c color/c]) (list/c real? real? real?)
  (match c
    [(? color%?)  (list (send c red) (send c green) (send c blue))]
    [(? string?)  (define color (send the-color-database find-color c))
                  (when (not color) (error 'decode-color "unknown color name ~e" c))
                  (->color color)]
    [(list (? real?) (? real?) (? real?))  c]
    [(? symbol?)  (->color (symbol->string c))]
    [_  (error '->color "unable to convert to color triple: ~e" c)]))

(define (color->color% c)
  (match-define (list r g b) c)
  (make-color% (real->color-byte r) (real->color-byte g) (real->color-byte b)))

(define (rgb->hsv rgb)
  (match-define (list r g b) (map (λ (x) (/ x 255)) rgb))
  (define mx (max r g b))
  (define mn (min r g b))
  (define c (- mx mn))
  (define h (* 60 (cond [(zero? c)  0]
                        [(= mx r)   (/ (- g b) c)]
                        [(= mx g)   (+ (/ (- b r) c) 2)]
                        [(= mx b)   (+ (/ (- r g) c) 4)])))
  (list (if (h . < . 0) (+ h 360) h)
        (if (zero? mx) 0 (/ c mx))
        mx))

(define (hsv->rgb hsv)
  (match-define (list h s v) hsv)
  (define c (* v s))
  (let ([h  (/ (real-modulo h 360) 60)])
    (define x (* c (- 1 (abs (- (real-modulo h 2) 1)))))
    (define-values (r g b)
      (cond [(and (0 . <= . h) (h . < . 1))  (values c x 0)]
            [(and (1 . <= . h) (h . < . 2))  (values x c 0)]
            [(and (2 . <= . h) (h . < . 3))  (values 0 c x)]
            [(and (3 . <= . h) (h . < . 4))  (values 0 x c)]
            [(and (4 . <= . h) (h . < . 5))  (values x 0 c)]
            [(and (5 . <= . h) (h . < . 6))  (values c 0 x)]))
    (define m (- v c))
    (list (* 255 (+ r m))
          (* 255 (+ g m))
          (* 255 (+ b m)))))

(define (integer->hue n)
  (let ([n  (abs n)])
    (define i (+ (case (remainder n 6) [(0) 0] [(1) 2] [(2) 4] [(3) 1] [(4) 3] [(5) 5])
                 (* 6 3 (quotient n 6))))
    (remainder (* i 59) 360)))

(define (integer->gray-value n)
  (* 1/7 (remainder (abs n) 8)))

(define (integer->pen-color n)
  (define h (integer->hue n))
  (hsv->rgb (list (- h (* 25 (sin (* (/ h 360) (* 3 pi)))))
                  1
                  (+ 1/2 (* 1/6 (sin (* (/ h 360) (* 3 pi))))))))

(define (integer->brush-color n)
  (define h (integer->hue n))
  (hsv->rgb (list (let ([y  (* (/ (- (sqrt (+ (/ h 60) 2)) (sqrt 2))
                                  (- (sqrt 8) (sqrt 2)))
                               6)])
                    (- h (* 15 (sin (* (/ y 6) (* 3 pi))))))
                  (+ 3/16 (* 3/32 (sin (* (/ h 360) (* 2 pi)))))
                  1)))

(define (integer->gray-pen-color i)
  (make-list 3 (* 128 (expt (integer->gray-value i) 3/4))))

(define (integer->gray-brush-color i)
  (make-list 3 (+ 127 (* 128 (expt (- 1 (integer->gray-value i)) 3/4)))))

(define pen-colors
  (for/vector ([color  (in-list (append (list (integer->gray-pen-color 0))
                                        (build-list 120 integer->pen-color)
                                        (build-list 7 (λ (n) (integer->gray-pen-color (- 7 n))))))])
    (map real->color-byte color)))

(define brush-colors
  (for/vector ([color  (in-list (append (list (integer->gray-brush-color 0))
                                        (build-list 120 integer->brush-color)
                                        (build-list 7 (λ (n) (integer->gray-brush-color (- 7 n))))))])
    (map real->color-byte color)))

(defproc (->pen-color [c plot-color/c]) (list/c real? real? real?)
  (cond [(exact-integer? c)  (vector-ref pen-colors (modulo c 128))]
        [else                (->color c)]))

(defproc (->brush-color [c plot-color/c]) (list/c real? real? real?)
  (cond [(exact-integer? c)  (vector-ref brush-colors (modulo c 128))]
        [else                (->color c)]))

(defproc (->pen-style [s plot-pen-style/c]) symbol?
  (cond [(exact-integer? s)  (case (remainder (abs s) 5)
                               [(0)  'solid]
                               [(1)  'dot]
                               [(2)  'long-dash]
                               [(3)  'short-dash]
                               [(4)  'dot-dash])]
        [(symbol? s)  s]
        [else  (raise-type-error '->pen-style "symbol or integer" s)]))

(defproc (->brush-style [s plot-brush-style/c]) symbol?
  (cond [(exact-integer? s)  (case (remainder (abs s) 7)
                               [(0)  'solid]
                               [(1)  'bdiagonal-hatch]
                               [(2)  'fdiagonal-hatch]
                               [(3)  'crossdiag-hatch]
                               [(4)  'horizontal-hatch]
                               [(5)  'vertical-hatch]
                               [(6)  'cross-hatch])]
        [(symbol? s)  s]
        [else  (raise-type-error '->brush-style "symbol or integer" s)]))

;; ===================================================================================================
;; Color functions

(defproc (color-seq [c1 color/c] [c2 color/c] [num exact-nonnegative-integer?]
                    [#:start? start? boolean? #t]
                    [#:end? end? boolean? #t]) (listof (list/c real? real? real?))
  (match-define (list r1 g1 b1) (->color c1))
  (match-define (list r2 g2 b2) (->color c2))
  (define rs (linear-seq r1 r2 num #:start? start? #:end? end?))
  (define gs (linear-seq g1 g2 num #:start? start? #:end? end?))
  (define bs (linear-seq b1 b2 num #:start? start? #:end? end?))
  (map list rs gs bs))

(defproc (color-seq* [colors (listof color/c)] [num exact-nonnegative-integer?]
                     [#:start? start? boolean? #t]
                     [#:end? end? boolean? #t]) (listof (list/c real? real? real?))
  (when (empty? colors) (raise-type-error 'color-seq* "nonempty (listof plot-color/c)" colors))
  (match-define (list (list rs gs bs) ...) (map ->color colors))
  (let ([rs  (linear-seq* rs num #:start? start? #:end? end?)]
        [gs  (linear-seq* gs num #:start? start? #:end? end?)]
        [bs  (linear-seq* bs num #:start? start? #:end? end?)])
    (map list rs gs bs)))

;; Returns an alpha value b such that, if 
(defproc (alpha-expt [a (real-in 0 1)] [n (>/c 0)]) real?
  (- 1 (expt (- 1 a) n)))

;; ===================================================================================================
;; Subdividing nonlinearly transformed shapes

(define subdivide-fracs '(3/7 4/7 2/7 5/7 1/7 6/7))

(define (subdivide-line transform v1 v2)
  (let loop ([v1 v1] [v2 v2] [depth 10])
    (let/ec return
      (when (zero? depth) (return (list v1 v2)))
      
      (define dc-v1 (transform v1))
      (define dc-v2 (transform v2))
      (define dc-dv (v- dc-v2 dc-v1))
      (when ((vmag dc-dv) . <= . 3)
        (return (list v1 v2)))
      
      (define dv (v- v2 v1))
      (define-values (max-area vc)
        (for/fold ([max-area 0] [vc v1]) ([frac  (in-list subdivide-fracs)])
          (define test-vc (v+ (v* dv frac) v1))
          (define test-area (abs (vcross2 dc-dv (v- (transform test-vc) dc-v1))))
          (cond [(test-area . > . max-area)  (values test-area test-vc)]
                [else  (values max-area vc)])))
      (when (max-area . <= . 3) (return (list v1 v2)))
      
      ;(plot3d-subdivisions (+ (plot3d-subdivisions) 1))
      (append (loop v1 vc (- depth 1))
              (rest (loop vc v2 (- depth 1)))))))

(define (subdivide-lines transform vs)
  (append
   (append*
    (for/list ([v1  (in-list vs)] [v2  (in-list (rest vs))])
      (define line-vs (subdivide-line transform v1 v2))
      (take line-vs (sub1 (length line-vs)))))
   (list (last vs))))

(define (subdivide-polygon transform vs)
  (subdivide-lines transform (cons (last vs) vs)))

;; ===================================================================================================
;; Fixpoint margin computation

;; In calculating margins in 2d-plot-area% and 3d-plot-area%, we have a mutual dependence problem:
;; 1. We can't set the margins without knowing where the ticks and axis labels will be drawn.
;; 2. We can't determine the tick and label angles (and thus their vertexes) without the margins.

;; The margins could be solved exactly using algebra and trigonometry, but the solutions wouldn't
;; be robust, as small changes to the layout algorithms would invalidate them.

;; So we use a fixpoint solution: iterate
;; 1. Getting tick and label vertexes ('get-vs' below); then
;; 2. Calculating new margins by how far off the dc the vertexes would be.

;; As long as this process is monotone and bounded, the distance off the dc is zero in the limit. In
;; practice, only a few iterations drives this distance to less than 1 drawing unit.

(define (margin-fixpoint x-min x-max y-min y-max
                         init-left init-right init-top init-bottom
                         get-vs)
  (let/ec return
    (for/fold ([left init-left] [right init-right] [top init-top] [bottom init-bottom]
                                ) ([i  (in-range 3)])
      (match-define (list (vector xs ys) ...) (get-vs left right top bottom))
      (define param-x-min (apply min x-min xs))
      (define param-x-max (apply max (sub1 x-max) xs))
      (define param-y-min (apply min y-min ys))
      (define param-y-max (apply max (sub1 y-max) ys))
      
      (define new-left (round (+ left (- x-min param-x-min))))
      (define new-right (round (- right (- (sub1 x-max) param-x-max))))
      (define new-top (round (+ top (- y-min param-y-min))))
      (define new-bottom (round (- bottom (- (sub1 y-max) param-y-max))))
      
      ;; Not enough space?
      (define area-x-min (+ x-min new-left))
      (define area-x-max (- x-max new-right))
      (define area-y-min (+ y-min new-top))
      (define area-y-max (- y-max new-bottom))
      (when (or (area-x-min . > . area-x-max)
                (area-y-min . > . area-y-max))
        (return init-left init-right init-top init-bottom))
      
      ;; Early out: if the margins haven't changed much, another iteration won't change them more
      ;; (hopefully)
      (when (and (= left new-left) (= right new-right)
                 (= top new-top) (= bottom new-bottom))
        (return new-left new-right new-top new-bottom))
      
      (values new-left new-right new-top new-bottom))))

;; ===================================================================================================
;; Null device context (used for speed testing)

(define-syntax-rule (define-public-stubs val name ...)
  (begin (define/public (name . args) val) ...))

(define null-dc%
  (class* object% (dc<%>)
    (define color (make-object color% 0 0 0))
    (define font (make-object font% 8 'default))
    (define pen (make-object pen% color 1 'solid))
    (define brush (make-object brush% color 'solid))
    (define matrix (vector 1 0 0 0 1 0))
    (define transformation (vector matrix 0 0 0 0 0))
    (define-public-stubs transformation get-transformation)
    (define-public-stubs matrix get-initial-matrix)
    (define-public-stubs 'solid get-text-mode)
    (define-public-stubs color get-text-foreground get-text-background get-background)
    (define-public-stubs #t get-smoothing ok? start-doc glyph-exists?)
    (define-public-stubs #f get-clipping-region get-gl-context)
    (define-public-stubs 0 get-rotation get-char-height get-char-width)
    (define-public-stubs (values 0 0) get-origin get-scale get-size)
    (define-public-stubs font get-font)
    (define-public-stubs pen get-pen)
    (define-public-stubs brush get-brush)
    (define-public-stubs 1 get-alpha)
    (define-public-stubs (values 1 1) get-device-scale)
    (define-public-stubs (values 0 0 0 0) get-text-extent)
    (define-public-stubs (void)
      flush suspend-flush resume-flush
      start-page end-page end-doc
      set-transformation
      set-text-mode
      set-smoothing
      set-text-foreground
      set-text-background
      set-scale
      set-rotation
      set-origin
      set-initial-matrix
      set-font
      set-clipping-region
      set-clipping-rect
      set-brush
      set-pen
      set-alpha
      set-background
      draw-text
      draw-spline
      draw-line
      draw-lines
      draw-ellipse
      draw-rectangle
      draw-rounded-rectangle
      draw-polygon
      draw-point
      draw-path
      draw-bitmap-section
      draw-bitmap
      draw-arc
      copy clear erase
      cache-font-metrics-key
      transform rotate scale translate
      try-color)
    (super-new)))

;; ===================================================================================================
;; Visible faces of a 3D rectangle

(define (rect-visible-faces r theta)
  (match-define (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max)) r)
  (list 
   ;; Top (z-max) face
   (list (vector 0.0 0.0 1.0)
         (vector x-min y-min z-max) (vector x-max y-min z-max)
         (vector x-max y-max z-max) (vector x-min y-max z-max))
   ;; Front (y-min) face
   (if ((cos theta) . > . 0.0)
       (list (vector 0.0 -1.0 0.0)
             (vector x-min y-min z-min) (vector x-max y-min z-min)
             (vector x-max y-min z-max) (vector x-min y-min z-max))
       empty)
   ;; Back (y-max) face
   (if ((cos theta) . < . 0.0)
       (list (vector 0.0 1.0 0.0)
             (vector x-min y-max z-min) (vector x-max y-max z-min)
             (vector x-max y-max z-max) (vector x-min y-max z-max))
       empty)
   ;; Left (x-min) face
   (if ((sin theta) . > . 0.0)
       (list (vector -1.0 0.0 0.0)
             (vector x-min y-min z-min) (vector x-min y-max z-min)
             (vector x-min y-max z-max) (vector x-min y-min z-max))
       empty)
   ;; Right (x-max) face
   (if ((sin theta) . < . 0.0)
       (list (vector 1.0 0.0 0.0)
             (vector x-max y-min z-min) (vector x-max y-max z-min)
             (vector x-max y-max z-max) (vector x-max y-min z-max))
       empty)))

;; ===================================================================================================
;; Origin-neutral pen styles

(struct pen-style (length ps) #:transparent)

(define (make-pen-style diff-ps)
  (let* ([diff-ps  (map exact->inexact diff-ps)]
         [diff-ps  (if (even? (length diff-ps)) diff-ps (append diff-ps diff-ps))])
    (define ps (map exact->inexact (cumulative-sum diff-ps)))
    (define len (last ps))
    (pen-style len ps)))

(define long-dash-pen-style (make-pen-style '(5 4)))
(define short-dash-pen-style (make-pen-style '(3 2)))
(define dot-pen-style (make-pen-style '(1 2)))
(define dot-dash-pen-style (make-pen-style '(1 3 4 3)))

(define (scale-pen-style sty scale)
  (let ([scale  (exact->inexact scale)])
    (match-define (pen-style len ps) sty)
    (pen-style (fl* scale len) (map (λ (p) (fl* scale p)) ps))))

(define (cons-exact->inexact v)
  (match-define (cons x1 y1) v)
  (cons (exact->inexact x1) (exact->inexact y1)))

(define (cons=? v1 v2)
  (match-define (cons x1 y1) v1)
  (match-define (cons x2 y2) v2)
  (and (fl= x1 x2) (fl= y1 y2)))

(define (segment-reverse seg)
  (reverse (map reverse seg)))

(define (segment-join s1 s2)
  (match-let ([(list s1 ... a)  s1]
              [(list b s2 ...)  s2])
    (append s1 (list (append a (rest b))) s2)))

(define (join-styled-segments segments)
  (let ([segments  (filter (compose not empty?) segments)])
    (if (empty? segments)
        empty
        (match-let ([(cons current-segment segments)  segments])
          (let loop ([current-segment current-segment] [segments segments])
            (cond [(empty? segments)  (list current-segment)]
                  [else
                   (define lst (last (last current-segment)))
                   (match-let ([(cons segment segments)  segments])
                     (define fst (first (first segment)))
                     (cond [(cons=? lst fst)  (loop (segment-join current-segment segment) segments)]
                           [else  (cons current-segment (loop segment segments))]))]))))))

(define (styled-segment* x1 y1 x2 y2 sty pair)
  (match-define (pen-style len (cons p rest-ps)) sty)
  (define start-x (fl* len (flfloor (fl/ x1 len))))
  (define m (fl/ (fl- y2 y1) (fl- x2 x1)))
  (define b (fl- y1 (fl* m x1)))
  (let loop ([xa start-x] [base-x 0.0] [ps rest-ps] [on? #t] [res empty])
    (let-values ([(base-x ps)  (cond [(empty? ps)  (values (fl+ base-x len) rest-ps)]
                                     [else         (values base-x ps)])])
      (cond [(xa . fl>= . x2)  (reverse res)]
            [else
             (match-let ([(cons p ps)  ps])
               (define xb (fl+ start-x (fl+ p base-x)))
               (cond [(and on? (xb . fl>= . x1))
                      (define v (let ([xa  (flmax x1 xa)]
                                      [xb  (flmin x2 xb)])
                                  (define ya (if (fl= x1 xa) y1 (fl+ (fl* m xa) b)))
                                  (define yb (if (fl= x2 xb) y2 (fl+ (fl* m xb) b)))
                                  (list (pair xa ya) (pair xb yb))))
                      (loop xb base-x ps (not on?) (cons v res))]
                     [else  (loop xb base-x ps (not on?) res)]))]))))

(define (styled-segment x1 y1 x2 y2 sty)
  (define dx (flabs (fl- x2 x1)))
  (define dy (flabs (fl- y2 y1)))
  (cond [(and (fl= dx 0.0) (fl= dy 0.0))  (list (list (cons x1 y1) (cons x2 y2)))]
        [(dx . > . dy)
         (define reverse? (x1 . fl> . x2))
         (let-values ([(x1 y1)  (if reverse? (values x2 y2) (values x1 y1))]
                      [(x2 y2)  (if reverse? (values x1 y1) (values x2 y2))])
           (define segment (styled-segment* x1 y1 x2 y2 sty cons))
           (if reverse? (segment-reverse segment) segment))]
        [else
         (define reverse? (y1 . fl> . y2))
         (let-values ([(x1 y1)  (if reverse? (values x2 y2) (values x1 y1))]
                      [(x2 y2)  (if reverse? (values x1 y1) (values x2 y2))])
           (define segment (styled-segment* y1 x1 y2 x2 sty (λ (y x) (cons x y))))
           (if reverse? (segment-reverse segment) segment))]))

(define (symbol->style name style-sym)
  (case style-sym
    [(long-dash)   long-dash-pen-style]
    [(short-dash)  short-dash-pen-style]
    [(dot)         dot-pen-style]
    [(dot-dash)    dot-dash-pen-style]
    [else  (error name "unknown pen style ~e" style-sym)]))

(define (draw-line/pen-style dc x1 y1 x2 y2 style-sym)
  (case style-sym
    [(transparent)  (void)]
    [(solid)        (send dc draw-line x1 y1 x2 y2)]
    [else
     (let ([x1  (exact->inexact x1)]
           [y1  (exact->inexact y1)]
           [x2  (exact->inexact x2)]
           [y2  (exact->inexact y2)])
       (define sty (symbol->style 'draw-line style-sym))
       (define pen (send dc get-pen))
       (define scale (flmax 1.0 (exact->inexact (send pen get-width))))
       (define vss (styled-segment x1 y1 x2 y2 (scale-pen-style sty scale)))
       (for ([vs  (in-list vss)] #:when (not (empty? vs)))
         (match-define (list (cons xa ya) (cons xb yb)) vs)
         (send dc draw-line xa ya xb yb)))]))

(define (draw-lines* dc vs sty)
  (define vss
    (append* (join-styled-segments
              (for/list ([v1  (in-list vs)] [v2  (in-list (rest vs))])
                (match-define (cons x1 y1) v1)
                (match-define (cons x2 y2) v2)
                (styled-segment x1 y1 x2 y2 sty)))))
  (for ([vs  (in-list vss)])
    (match vs
      [(list (cons x1 y1) (cons x2 y2))  (send dc draw-line x1 y1 x2 y2)]
      [_  (send dc draw-lines vs)])))

(define (draw-lines/pen-style dc vs style-sym)
  (cond [(or (empty? vs) (eq? style-sym 'transparent))  (void)]
        [else
         (let ([vs  (map cons-exact->inexact vs)])
           (cond [(eq? style-sym 'solid)  (send dc draw-lines vs)]
                 [else
                  (define pen (send dc get-pen))
                  (define scale (flmax 1.0 (exact->inexact (send pen get-width))))
                  (define sty (scale-pen-style (symbol->style 'draw-lines style-sym) scale))
                  (draw-lines* dc vs sty)]))]))

;; ===================================================================================================
;; Drawing a bitmap using 2x supersampling

(define (draw-bitmap/supersampling draw width height)
  (define bm2 (make-bitmap (* 2 width) (* 2 height)))
  (define dc2 (make-object bitmap-dc% bm2))
  (send dc2 set-scale 2 2)
  (draw dc2)
  
  (define bm (make-bitmap width height))
  (define dc (make-object bitmap-dc% bm))
  (send dc set-scale 1/2 1/2)
  (send dc set-smoothing 'smoothed)
  (send dc draw-bitmap bm2 0 0)
  bm)

(define (draw-bitmap draw width height)
  (define bm (make-bitmap width height))
  (define dc (make-object bitmap-dc% bm))
  (draw dc)
  bm)
