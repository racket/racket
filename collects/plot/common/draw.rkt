#lang racket/base

;; Extra drawing, font, color and style functions.

(require racket/draw racket/class racket/match racket/list racket/contract racket/math
         "math.rkt"
         "contract.rkt"
         "contract-doc.rkt"
         "sample.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Drawing text rotated around an anchor point

(define (draw-text/anchor dc str x y [anchor 'top-left] [combine? #f] [offset 0] [angle 0])
  (define-values (width height _1 _2) (send dc get-text-extent str #f combine? offset))
  (define dx (case anchor
               [(top-left left bottom-left)     0]
               [(top center bottom)             (* 1/2 width)]
               [(top-right right bottom-right)  width]
               [else  (raise-type-error 'draw-text/anchor "anchor/c" anchor)]))
  (define dy (case anchor
               [(top-left top top-right)           0]
               [(left center right)                (* 1/2 height)]
               [(bottom-left bottom bottom-right)  height]))
  (define rdx (+ (* (sin angle) dy) (* (cos angle) dx)))
  (define rdy (- (* (cos angle) dy) (* (sin angle) dx)))
  
  (send dc draw-text str (- x rdx) (- y rdy) combine? offset angle))

(define (get-text-corners/anchor dc str x y [anchor 'top-left] [combine? #f] [offset 0] [angle 0])
  (define-values (width height _1 _2) (send dc get-text-extent str #f combine? offset))
  (define dxs (case anchor
                [(top-left left bottom-left)     (list 0 width)]
                [(top center bottom)             (list (* -1/2 width) (* 1/2 width))]
                [(top-right right bottom-right)  (list (- width) 0)]
                [else  (raise-type-error 'get-text-corners/anchor "anchor/c" anchor)]))
  (define dys (case anchor
                [(top-left top top-right)           (list 0 height)]
                [(left center right)                (list (* -1/2 height) (* 1/2 width))]
                [(bottom-left bottom bottom-right)  (list (- height) 0)]))
  
  (for*/list ([dx  (in-list dxs)] [dy  (in-list dys)])
    (define rdx (+ (* (sin angle) dy) (* (cos angle) dx)))
    (define rdy (- (* (cos angle) dy) (* (sin angle) dx)))
    (vector (+ x rdx) (+ y rdy))))

;; ===================================================================================================
;; Draw paramter normalization

(define (real->font-size size)
  (define i (inexact->exact (round size)))
  (min (max i 1) 255))

(define (real->color-byte f)
  (define i (inexact->exact (floor f)))
  (min (max i 0) 255))

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
  (make-object color% (real->color-byte r) (real->color-byte g) (real->color-byte b)))

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

#|
(define pen-colors
  '#((0 0 0)          ; black
     (128 0 0)        ; red
     (0 96 0)         ; green
     (0 0 160)        ; blue
     (192 96 0)       ; yellow
     (0 112 128)      ; cyan
     (160 32 240)     ; magenta
     (160 160 160)))  ; gray

(defproc (->pen-color [c plot-color/c]) (list/c real? real? real?)
  (cond [(exact-integer? c)  (vector-ref pen-colors (modulo c 8))]
        [else                (->color c)]))

(define brush-colors
  '#((255 255 255)    ; white
     (255 192 192)    ; red
     (192 255 192)    ; green
     (212 224 240)    ; blue
     (255 248 192)    ; yellow
     (192 240 255)    ; cyan
     (240 224 255)    ; magenta
     (212 212 212)))  ; gray

(defproc (->brush-color [c plot-color/c]) (list/c real? real? real?)
  (cond [(exact-integer? c)  (vector-ref brush-colors (modulo c 8))]
        [else                (->color c)]))
|#

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

(defproc (maybe-apply/list [list-or-proc (or/c (listof any/c) (any/c . -> . any/c))]
                           [xs (listof any/c)]) (listof any/c)
  (cond [(procedure? list-or-proc)  (list-or-proc xs)]
        [else                       list-or-proc]))

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

(define (appx= x y) ((abs (- x y)) . < . 1/2))

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
      
      (define new-left (+ left (- x-min param-x-min)))
      (define new-right (- right (- (sub1 x-max) param-x-max)))
      (define new-top (+ top (- y-min param-y-min)))
      (define new-bottom (- bottom (- (sub1 y-max) param-y-max)))
      
      ;; Early out: if the margins haven't changed much, another iteration won't change them more
      ;; (hopefully)
      (when (and (appx= left new-left) (appx= right new-right)
                 (appx= top new-top) (appx= bottom new-bottom))
        (return new-left new-right new-top new-bottom))
      
      (values new-left new-right new-top new-bottom))))
