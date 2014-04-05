#lang racket/base

(require racket/class racket/match racket/list racket/contract
         unstable/latent-contract/defthing
         unstable/contract
         plot/utils
         "../common/utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================

(define ((lines3d-render-proc vs-fun color width style alpha label) area)
  (send area put-alpha alpha)
  (send area put-pen color width style)
  (send area put-lines (vs-fun))
  
  (cond [label  (line-legend-entry label color width style)]
        [else  empty]))

(define (lines3d-renderer
         vs-thnk x-min x-max y-min y-max z-min z-max color width style alpha label)
  (define rvs (filter vrational? (vs-thnk)))
  (cond [(empty? rvs)  (renderer3d #f #f #f #f)]
        [else
         (match-define (list (vector rxs rys rzs) ...) rvs)
         (let ([x-min  (if x-min x-min (apply min* rxs))]
               [x-max  (if x-max x-max (apply max* rxs))]
               [y-min  (if y-min y-min (apply min* rys))]
               [y-max  (if y-max y-max (apply max* rys))]
               [z-min  (if z-min z-min (apply min* rzs))]
               [z-max  (if z-max z-max (apply max* rzs))])
           (renderer3d (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max)) #f
                       default-ticks-fun
                       (lines3d-render-proc vs-thnk color width style alpha label)))]))

(defproc (lines3d
          [vs  (sequence/c (sequence/c #:min-count 3 real?))]
          [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
          [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
          [#:z-min z-min (or/c rational? #f) #f] [#:z-max z-max (or/c rational? #f) #f]
          [#:color color plot-color/c (line-color)]
          [#:width width (>=/c 0) (line-width)]
          [#:style style plot-pen-style/c (line-style)]
          [#:alpha alpha (real-in 0 1) (line-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?
  (let ([vs  (sequence->listof-vector 'lines3d vs 3)])
    (lines3d-renderer (λ () vs) x-min x-max y-min y-max z-min z-max color width style alpha label)))

(defproc (parametric3d
          [f (real? . -> . (sequence/c real?))]
          [t-min rational?] [t-max rational?]
          [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
          [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
          [#:z-min z-min (or/c rational? #f) #f] [#:z-max z-max (or/c rational? #f) #f]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (line-samples)]
          [#:color color plot-color/c (line-color)]
          [#:width width (>=/c 0) (line-width)]
          [#:style style plot-pen-style/c (line-style)]
          [#:alpha alpha (real-in 0 1) (line-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?
  (let ([f  (λ (t) (sequence-head-vector 'parametric3d (f t) 3))])
    (lines3d-renderer (λ () (map f (linear-seq t-min t-max (animated-samples samples))))
                      x-min x-max y-min y-max z-min z-max color width style alpha label)))
