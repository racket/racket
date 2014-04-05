#lang racket/base

(require racket/class racket/list racket/match racket/contract
         unstable/latent-contract/defthing
         unstable/contract
         plot/utils
         "../common/utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================

(define ((points3d-render-proc vs sym color fill-color size line-width alpha label) area)
  (send area put-alpha alpha)
  (send area put-pen color line-width 'solid)
  (send area put-brush fill-color 'solid)
  (send area put-glyphs vs sym size)
  
  (cond [label  (point-legend-entry label sym color fill-color size line-width)]
        [else   empty]))

(defproc (points3d
          [vs  (sequence/c (sequence/c #:min-count 3 real?))]
          [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
          [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
          [#:z-min z-min (or/c rational? #f) #f] [#:z-max z-max (or/c rational? #f) #f]
          [#:sym sym point-sym/c (point-sym)]
          [#:color color plot-color/c (point-color)]
          [#:fill-color fill-color (or/c plot-color/c 'auto) 'auto]
          [#:size size (>=/c 0) (point-size)]
          [#:line-width line-width (>=/c 0) (point-line-width)]
          [#:alpha alpha (real-in 0 1) (point-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?
  (let* ([vs  (sequence->listof-vector 'points3d vs 3)]
         [vs  (filter vrational? vs)])
    (cond [(empty? vs)  (renderer3d #f #f #f #f)]
          [else
           (match-define (list (vector xs ys zs) ...) vs)
           (let ([x-min  (if x-min x-min (apply min* xs))]
                 [x-max  (if x-max x-max (apply max* xs))]
                 [y-min  (if y-min y-min (apply min* ys))]
                 [y-max  (if y-max y-max (apply max* ys))]
                 [z-min  (if z-min z-min (apply min* zs))]
                 [z-max  (if z-max z-max (apply max* zs))])
             (renderer3d
              (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max)) #f default-ticks-fun
              (points3d-render-proc vs sym color (cond [(eq? fill-color 'auto)  (->pen-color color)]
                                                       [else  fill-color])
                                    size line-width alpha label)))])))

;; ===================================================================================================

(define ((vector-field3d-render-fun f samples scale color line-width line-style alpha label) area)
  (match-define (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max))
    (send area get-bounds-rect))
  
  (define xs0 (linear-seq x-min x-max samples #:start? #t #:end? #t))
  (define ys0 (linear-seq y-min y-max samples #:start? #t #:end? #t))
  (define zs0 (linear-seq z-min z-max samples #:start? #t #:end? #t))
  
  (define-values (vs dxs dys dzs norms mags)
    (for*/lists (vs dxs dys dzs norms mags) ([x   (in-list xs0)]
                                                   [y   (in-list ys0)]
                                                   [z   (in-list zs0)]
                                                   [dv  (in-value (f x y z))] #:when (vrational? dv))
      (match-define (vector dx dy dz) dv)
      (values (vector x y z) dx dy dz (vnormalize dv) (vmag dv))))
  
  (cond [(empty? vs)  empty]
        [else (define box-x-size (/ (- x-max x-min) samples))
              (define box-y-size (/ (- y-max y-min) samples))
              (define box-z-size (/ (- z-max z-min) samples))
              
              (define new-mags
                (match scale
                  [(? real?)  (map (λ (mag) (* scale mag)) mags)]
                  ['normalized  (define box-size (min box-x-size box-y-size box-z-size))
                                (build-list (length dxs) (λ _ box-size))]
                  ['auto  (define dx-max (apply max (map abs dxs)))
                          (define dy-max (apply max (map abs dys)))
                          (define dz-max (apply max (map abs dzs)))
                          (define scale (min (/ box-x-size dx-max)
                                             (/ box-y-size dy-max)
                                             (/ box-z-size dz-max)))
                          (map (λ (mag) (* scale mag)) mags)]))
              
              (send area put-alpha alpha)
              (send area put-pen color line-width line-style)
              (for ([v     (in-list vs)]
                    [norm  (in-list norms)]
                    [mag   (in-list new-mags)])
                (send area put-arrow v (v+ v (v* norm mag))))
              
              (cond [label  (arrow-legend-entry label color line-width line-style)]
                    [else   empty])]))

(defproc (vector-field3d
          [f (or/c (real? real? real? . -> . (sequence/c real?))
                   ((vector/c real? real? real?) . -> . (sequence/c real?)))]
          [x-min (or/c rational? #f) #f] [x-max (or/c rational? #f) #f]
          [y-min (or/c rational? #f) #f] [y-max (or/c rational? #f) #f]
          [z-min (or/c rational? #f) #f] [z-max (or/c rational? #f) #f]
          [#:samples samples exact-positive-integer? ( vector-field3d-samples)]
          [#:scale scale (or/c real? (one-of/c 'auto 'normalized)) (vector-field-scale)]
          [#:color color plot-color/c (vector-field-color)]
          [#:line-width line-width (>=/c 0) (vector-field-line-width)]
          [#:line-style line-style plot-pen-style/c (vector-field-line-style)]
          [#:alpha alpha (real-in 0 1) (vector-field-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?
  (let ([f  (cond [(procedure-arity-includes? f 3 #t)
                   (λ (x y z) (sequence-head-vector 'vector-field3d (f x y z) 3))]
                  [else
                   (λ (x y z) (sequence-head-vector 'vector-field3d (f (vector x y z)) 3))])])
    (renderer3d (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max)) #f default-ticks-fun
                (vector-field3d-render-fun f samples scale color line-width line-style alpha label))))
