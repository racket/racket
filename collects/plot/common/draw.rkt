#lang racket/base

;; Extra drawing, font, color and style functions.

(require racket/draw racket/class racket/match racket/list racket/contract
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
  (cond [(exact-integer? c)  (vector-ref pen-colors (remainder (abs c) 8))]
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
  (cond [(exact-integer? c)  (vector-ref brush-colors (remainder (abs c) 8))]
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

(defproc (maybe-apply/list [list-or-proc (or/c (listof any/c) (any/c . -> . any/c))]
                           [xs (listof any/c)]) (listof any/c)
  (cond [(procedure? list-or-proc)  (list-or-proc xs)]
        [else                       list-or-proc]))
