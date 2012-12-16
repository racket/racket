#lang racket/base

;; Line renderers.

(require racket/contract racket/class racket/match racket/math racket/list racket/sequence
         unstable/latent-contract/defthing
         unstable/contract
         plot/utils
         "../common/utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Lines, parametric, polar

(define ((lines-render-proc vs color width style alpha label) area)
  (send area put-alpha alpha)
  (send area put-pen color width style)
  (send area put-lines vs)
  
  (cond [label  (line-legend-entry label color width style)]
        [else   empty]))

(defproc (lines [vs  (sequence/c (sequence/c real?))]
                [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
                [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
                [#:color color plot-color/c (line-color)]
                [#:width width (>=/c 0) (line-width)]
                [#:style style plot-pen-style/c (line-style)]
                [#:alpha alpha (real-in 0 1) (line-alpha)]
                [#:label label (or/c string? #f) #f]
                ) renderer2d?
  (let ([vs  (sequence->listof-vector 'lines vs 2)])
    (define rvs (filter vrational? vs))
    (cond [(empty? rvs)  (renderer2d #f #f #f #f)]
          [else
           (match-define (list (vector rxs rys) ...) rvs)
           (let ([x-min  (if x-min x-min (apply min* rxs))]
                 [x-max  (if x-max x-max (apply max* rxs))]
                 [y-min  (if y-min y-min (apply min* rys))]
                 [y-max  (if y-max y-max (apply max* rys))])
             (renderer2d (vector (ivl x-min x-max) (ivl y-min y-max)) #f default-ticks-fun
                         (lines-render-proc vs color width style alpha label)))])))

(defproc (parametric [f (real? . -> . (sequence/c real?))]
                     [t-min rational?] [t-max rational?]
                     [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
                     [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
                     [#:samples samples (and/c exact-integer? (>=/c 2)) (line-samples)]
                     [#:color color plot-color/c (line-color)]
                     [#:width width (>=/c 0) (line-width)]
                     [#:style style plot-pen-style/c (line-style)]
                     [#:alpha alpha (real-in 0 1) (line-alpha)]
                     [#:label label (or/c string? #f) #f]
                     ) renderer2d?
  (let ([f  (λ (t) (sequence-head-vector 'parametric (f t) 2))])
    (lines (map f (linear-seq t-min t-max samples))
           #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
           #:color color #:width width #:style style #:alpha alpha
           #:label label)))

(defproc (polar [f (real? . -> . real?)]
                [θ-min real? 0] [θ-max real? (* 2 pi)]
                [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
                [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
                [#:samples samples (and/c exact-integer? (>=/c 2)) (line-samples)]
                [#:color color plot-color/c (line-color)]
                [#:width width (>=/c 0) (line-width)]
                [#:style style plot-pen-style/c (line-style)]
                [#:alpha alpha (real-in 0 1) (line-alpha)]
                [#:label label (or/c string? #f) #f]
                ) renderer2d?
  (lines (let ([θs  (linear-seq θ-min θ-max samples)])
           (map polar->cartesian θs (map* f θs)))
         #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
         #:color color #:width width #:style style #:alpha alpha
         #:label label))

;; ===================================================================================================
;; Function

(define ((function-render-proc f samples color width style alpha label) area)
  (match-define (vector x-ivl y-ivl) (send area get-bounds-rect))
  (match-define (sample xs ys y-min y-max) (f x-ivl samples))
  
  (send area put-alpha alpha)
  (send area put-pen color width style)
  (send area put-lines (map vector xs ys))
  
  (cond [label  (line-legend-entry label color width style)]
        [else   empty]))

(defproc (function [f (real? . -> . real?)]
                   [x-min (or/c rational? #f) #f] [x-max (or/c rational? #f) #f]
                   [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
                   [#:samples samples (and/c exact-integer? (>=/c 2)) (line-samples)]
                   [#:color color plot-color/c (line-color)]
                   [#:width width (>=/c 0) (line-width)]
                   [#:style style plot-pen-style/c (line-style)]
                   [#:alpha alpha (real-in 0 1) (line-alpha)]
                   [#:label label (or/c string? #f) #f]
                   ) renderer2d?
  (define x-ivl (ivl x-min x-max))
  (define y-ivl (ivl y-min y-max))
  (let ([f  (function->sampler f x-ivl)])
    (renderer2d (vector x-ivl y-ivl)
                (function-bounds-fun f samples)
                default-ticks-fun
                (function-render-proc f samples color width style alpha label))))

;; ===================================================================================================
;; Inverse function

(define ((inverse-render-proc f samples color width style alpha label) area)
  (match-define (vector x-ivl y-ivl) (send area get-bounds-rect))
  (match-define (sample ys xs x-min x-max) (f y-ivl samples))
  
  (send area put-alpha alpha)
  (send area put-pen color width style)
  (send area put-lines (map vector xs ys))
  
  (cond [label  (line-legend-entry label color width style)]
        [else   empty]))

(defproc (inverse [f (real? . -> . real?)]
                  [y-min (or/c rational? #f) #f] [y-max (or/c rational? #f) #f]
                  [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
                  [#:samples samples (and/c exact-integer? (>=/c 2)) (line-samples)]
                  [#:color color plot-color/c (line-color)]
                  [#:width width (>=/c 0) (line-width)]
                  [#:style style plot-pen-style/c (line-style)]
                  [#:alpha alpha (real-in 0 1) (line-alpha)]
                  [#:label label (or/c string? #f) #f]
                  ) renderer2d?
  (define x-ivl (ivl x-min x-max))
  (define y-ivl (ivl y-min y-max))
  (define g (inverse->sampler f y-ivl))
  (renderer2d (vector x-ivl y-ivl)
              (inverse-bounds-fun g samples)
              default-ticks-fun
              (inverse-render-proc g samples color width style alpha label)))

;; ===================================================================================================
;; Kernel density estimation

(defproc (density [xs (sequence/c real?)] [bw-adjust real? 1]
                  [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
                  [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
                  [#:samples samples (and/c exact-integer? (>=/c 2)) (line-samples)]
                  [#:color color plot-color/c (line-color)]
                  [#:width width (>=/c 0) (line-width)]
                  [#:style style plot-pen-style/c (line-style)]
                  [#:alpha alpha (real-in 0 1) (line-alpha)]
                  [#:label label (or/c string? #f) #f]
                  ) renderer2d?
  (let ([xs  (sequence->list xs)])
    (define n (length xs))
    (define sd (sqrt (- (/ (sum sqr xs) n) (sqr (/ (sum values xs) n)))))
    (define h (* bw-adjust 1.06 sd (expt n -0.2)))
    (define-values (f fx-min fx-max) (kde xs h))
    (let ([x-min  (if x-min x-min fx-min)]
          [x-max  (if x-max x-max fx-max)])
      (function f x-min x-max #:y-min y-min #:y-max y-max #:samples samples
                #:color color #:width width #:style style #:alpha alpha #:label label))))
