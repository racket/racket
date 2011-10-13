#lang racket/base

(require racket/class racket/match racket/list racket/contract
         "../common/math.rkt"
         "../common/vector.rkt"
         "../common/contract.rkt" "../common/contract-doc.rkt"
         "../common/legend.rkt"
         "../common/sample.rkt"
         "../common/parameters.rkt"
         "renderer.rkt"
         "sample.rkt")

(provide lines3d parametric3d)

;; ===================================================================================================

(define ((lines3d-render-proc vs-fun color width style alpha label) area)
  (send area put-alpha alpha)
  (send area put-pen color width style)
  (send area put-lines (vs-fun))
  
  (cond [label  (line-legend-entry label color width style)]
        [else  empty]))

(define (lines3d-renderer
         vs-thnk x-min x-max y-min y-max z-min z-max color width style alpha label)
  (define rvs (filter vregular? (vs-thnk)))
  (cond [(empty? rvs)  null-renderer3d]
        [else
         (match-define (list (vector rxs rys rzs) ...) rvs)
         (let ([x-min  (if x-min x-min (apply min* rxs))]
               [x-max  (if x-max x-max (apply max* rxs))]
               [y-min  (if y-min y-min (apply min* rys))]
               [y-max  (if y-max y-max (apply max* rys))]
               [z-min  (if z-min z-min (apply min* rzs))]
               [z-max  (if z-max z-max (apply max* rzs))])
           (renderer3d (lines3d-render-proc vs-thnk color width style alpha label)
                       default-3d-ticks-fun
                       null-3d-bounds-fun
                       x-min x-max y-min y-max z-min z-max))]))

(defproc (lines3d
          [vs  (listof (vector/c real? real? real?))]
          [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
          [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
          [#:z-min z-min (or/c real? #f) #f] [#:z-max z-max (or/c real? #f) #f]
          [#:color color plot-color/c (line-color)]
          [#:width width (>=/c 0) (line-width)]
          [#:style style plot-pen-style/c (line-style)]
          [#:alpha alpha (real-in 0 1) (line-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?
  (lines3d-renderer (λ () vs) x-min x-max y-min y-max z-min z-max color width style alpha label))

(defproc (parametric3d
          [f (real? . -> . (vector/c real? real? real?))]
          [t-min real?] [t-max real?]
          [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
          [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
          [#:z-min z-min (or/c real? #f) #f] [#:z-max z-max (or/c real? #f) #f]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (line-samples)]
          [#:color color plot-color/c (line-color)]
          [#:width width (>=/c 0) (line-width)]
          [#:style style plot-pen-style/c (line-style)]
          [#:alpha alpha (real-in 0 1) (line-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?
  (lines3d-renderer (λ () (sample-parametric f t-min t-max (animated-samples samples)))
                    x-min x-max y-min y-max z-min z-max color width style alpha label))
