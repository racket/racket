#lang racket/base

;; Renderers for intervals between functions.

(require racket/contract racket/class racket/match racket/math racket/list
         unstable/latent-contract/defthing
         unstable/contract
         plot/utils
         "../common/utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Lines, parametric, polar

(define ((lines-interval-render-proc v1s v2s color style
                                     line1-color line1-width line1-style
                                     line2-color line2-width line2-style
                                     alpha label)
         area)
  (send area put-alpha alpha)
  (send area put-pen 0 0 'transparent)
  (send area put-brush color style)
  (send area put-polygon (append v1s (reverse v2s)))
  
  (send area put-pen line1-color line1-width line1-style)
  (send area put-lines v1s)
  
  (send area put-pen line2-color line2-width line2-style)
  (send area put-lines v2s)
  
  (cond [label  (interval-legend-entry label color style 0 0 'transparent
                                       line1-color line1-width line1-style
                                       line2-color line2-width line2-style)]
        [else  empty]))

(defproc (lines-interval
          [v1s (sequence/c (sequence/c #:min-count 2 real?))]
          [v2s (sequence/c (sequence/c #:min-count 2 real?))]
          [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
          [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
          [#:color color plot-color/c (interval-color)]
          [#:style style plot-brush-style/c (interval-style)]
          [#:line1-color line1-color plot-color/c (interval-line1-color)]
          [#:line1-width line1-width (>=/c 0) (interval-line1-width)]
          [#:line1-style line1-style plot-pen-style/c (interval-line1-style)]
          [#:line2-color line2-color plot-color/c (interval-line2-color)]
          [#:line2-width line2-width (>=/c 0) (interval-line2-width)]
          [#:line2-style line2-style plot-pen-style/c (interval-line2-style)]
          [#:alpha alpha (real-in 0 1) (interval-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer2d?
  (let ([v1s  (sequence->listof-vector 'lines-interval v1s 2)]
        [v2s  (sequence->listof-vector 'lines-interval v2s 2)])
    (define rvs (filter vrational? (append v1s v2s)))
    (cond
      [(empty? rvs)  (renderer2d #f #f #f #f)]
      [else
       (match-define (list (vector rxs rys) ...) rvs)
       (let ([x-min  (if x-min x-min (apply min* rxs))]
             [x-max  (if x-max x-max (apply max* rxs))]
             [y-min  (if y-min y-min (apply min* rys))]
             [y-max  (if y-max y-max (apply max* rys))])
         (renderer2d (vector (ivl x-min x-max) (ivl y-min y-max)) #f default-ticks-fun
                     (lines-interval-render-proc v1s v2s color style
                                                 line1-color line1-width line1-style
                                                 line2-color line2-width line2-style
                                                 alpha label)))])))

(defproc (parametric-interval
          [f1 (real? . -> . (sequence/c real?))]
          [f2 (real? . -> . (sequence/c real?))]
          [t-min rational?] [t-max rational?]
          [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
          [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (line-samples)]
          [#:color color plot-color/c (interval-color)]
          [#:style style plot-brush-style/c (interval-style)]
          [#:line1-color line1-color plot-color/c (interval-line1-color)]
          [#:line1-width line1-width (>=/c 0) (interval-line1-width)]
          [#:line1-style line1-style plot-pen-style/c (interval-line1-style)]
          [#:line2-color line2-color plot-color/c (interval-line2-color)]
          [#:line2-width line2-width (>=/c 0) (interval-line2-width)]
          [#:line2-style line2-style plot-pen-style/c (interval-line2-style)]
          [#:alpha alpha (real-in 0 1) (interval-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer2d?
  (let ([f1  (λ (t) (sequence-head-vector 'parametric-interval (f1 t) 2))]
        [f2  (λ (t) (sequence-head-vector 'parametric-interval (f2 t) 2))])
    (lines-interval
     (map f1 (linear-seq t-min t-max samples))
     (map f2 (linear-seq t-min t-max samples))
     #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
     #:color color #:style style
     #:line1-color line1-color #:line1-width line1-width #:line1-style line1-style
     #:line2-color line2-color #:line2-width line2-width #:line2-style line2-style
     #:alpha alpha #:label label)))

(defproc (polar-interval
          [f1 (real? . -> . real?)] [f2 (real? . -> . real?)]
          [θ-min rational? 0] [θ-max rational? (* 2 pi)]
          [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
          [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (line-samples)]
          [#:color color plot-color/c (interval-color)]
          [#:style style plot-brush-style/c (interval-style)]
          [#:line1-color line1-color plot-color/c (interval-line1-color)]
          [#:line1-width line1-width (>=/c 0) (interval-line1-width)]
          [#:line1-style line1-style plot-pen-style/c (interval-line1-style)]
          [#:line2-color line2-color plot-color/c (interval-line2-color)]
          [#:line2-width line2-width (>=/c 0) (interval-line2-width)]
          [#:line2-style line2-style plot-pen-style/c (interval-line2-style)]
          [#:alpha alpha (real-in 0 1) (interval-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer2d?
  (define θs (linear-seq θ-min θ-max samples))
  (lines-interval
   (map polar->cartesian θs (map* f1 θs))
   (map polar->cartesian θs (map* f2 θs))
   #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
   #:color color #:style style
   #:line1-color line1-color #:line1-width line1-width #:line1-style line1-style
   #:line2-color line2-color #:line2-width line2-width #:line2-style line2-style
   #:alpha alpha #:label label))

;; ===================================================================================================
;; Function

(define ((function-interval-render-proc f1 f2 samples color style
                                        line1-color line1-width line1-style
                                        line2-color line2-width line2-style
                                        alpha label)
         area)
  (match-define (vector x-ivl y-ivl) (send area get-bounds-rect))
  (match-define (sample x1s y1s _ _) (f1 x-ivl samples))
  (match-define (sample x2s y2s _ _) (f2 x-ivl samples))
  (define v1s (map vector x1s y1s))
  (define v2s (map vector x2s y2s))
  
  ((lines-interval-render-proc v1s v2s color style
                               line1-color line1-width line1-style
                               line2-color line2-width line2-style
                               alpha label)
   area))

(defproc (function-interval
          [f1 (real? . -> . real?)] [f2 (real? . -> . real?)]
          [x-min (or/c rational? #f) #f] [x-max (or/c rational? #f) #f]
          [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (line-samples)]
          [#:color color plot-color/c (interval-color)]
          [#:style style plot-brush-style/c (interval-style)]
          [#:line1-color line1-color plot-color/c (interval-line1-color)]
          [#:line1-width line1-width (>=/c 0) (interval-line1-width)]
          [#:line1-style line1-style plot-pen-style/c (interval-line1-style)]
          [#:line2-color line2-color plot-color/c (interval-line2-color)]
          [#:line2-width line2-width (>=/c 0) (interval-line2-width)]
          [#:line2-style line2-style plot-pen-style/c (interval-line2-style)]
          [#:alpha alpha (real-in 0 1) (interval-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer2d?
  (define x-ivl (ivl x-min x-max))
  (define y-ivl (ivl y-min y-max))
  (define g1 (function->sampler f1 x-ivl))
  (define g2 (function->sampler f2 x-ivl))
  (renderer2d (vector x-ivl y-ivl)
              (function-interval-bounds-fun g1 g2 samples)
              default-ticks-fun
              (function-interval-render-proc g1 g2 samples color style
                                             line1-color line1-width line1-style
                                             line2-color line2-width line2-style
                                             alpha label)))

;; ===================================================================================================
;; Inverse function

(define ((inverse-interval-render-proc f1 f2 samples color style
                                       line1-color line1-width line1-style
                                       line2-color line2-width line2-style
                                       alpha label)
         area)
  (match-define (vector x-ivl y-ivl) (send area get-bounds-rect))
  (match-define (sample y1s x1s _ _) (f1 y-ivl samples))
  (match-define (sample y2s x2s _ _) (f2 y-ivl samples))
  (define v1s (map vector x1s y1s))
  (define v2s (map vector x2s y2s))
  
  ((lines-interval-render-proc v1s v2s color style
                               line1-color line1-width line1-style
                               line2-color line2-width line2-style
                               alpha label)
   area))

(defproc (inverse-interval
          [f1 (real? . -> . real?)] [f2 (real? . -> . real?)]
          [y-min (or/c rational? #f) #f] [y-max (or/c rational? #f) #f]
          [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (line-samples)]
          [#:color color plot-color/c (interval-color)]
          [#:style style plot-brush-style/c (interval-style)]
          [#:line1-color line1-color plot-color/c (interval-line1-color)]
          [#:line1-width line1-width (>=/c 0) (interval-line1-width)]
          [#:line1-style line1-style plot-pen-style/c (interval-line1-style)]
          [#:line2-color line2-color plot-color/c (interval-line2-color)]
          [#:line2-width line2-width (>=/c 0) (interval-line2-width)]
          [#:line2-style line2-style plot-pen-style/c (interval-line2-style)]
          [#:alpha alpha (real-in 0 1) (interval-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer2d?
  (define x-ivl (ivl x-min x-max))
  (define y-ivl (ivl y-min y-max))
  (define g1 (inverse->sampler f1 y-ivl))
  (define g2 (inverse->sampler f2 y-ivl))
  (renderer2d (vector x-ivl y-ivl)
              (inverse-interval-bounds-fun g1 g2 samples)
              default-ticks-fun
              (inverse-interval-render-proc g1 g2 samples color style
                                            line1-color line1-width line1-style
                                            line2-color line2-width line2-style
                                            alpha label)))
