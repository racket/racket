#lang racket/base

;; A compatibility module for the old 'plot'.

(require racket/contract racket/class racket/snip racket/draw racket/vector
         unstable/latent-contract unstable/latent-contract/defthing
         ;; Plotting
         plot/private/common/math
         plot/private/common/contract
         plot/private/common/plot-element
         plot/private/plot2d/plot-area
         plot/private/plot3d/plot-area
         (prefix-in new. (only-in plot
                                  x-axis y-axis
                                  plot-x-ticks plot-y-ticks plot-z-ticks
                                  points error-bars vector-field
                                  plot-title plot-x-label plot-y-label plot-z-label
                                  plot-foreground plot-background
                                  plot3d-angle plot3d-altitude))
         plot/private/deprecated/renderers
         ;; Miscellaneous
         plot/private/deprecated/math)

(provide mix
         (activate-contract-out plot-color?
                                plot plot3d
                                points vector-field error-bars
                                line
                                contour shade
                                surface)
         (only-doc-out (all-defined-out))
         ;; Miscellaneous
         make-vec derivative gradient)

(define (mix . data)
  (for/fold ([f  (λ (area) (void))]) ([d  (in-list data)])
    (λ (area)
      (f area)
      (d area)
      (void))))

(defproc (plot-color? [v any/c]) boolean?
  (and (member v '(white black yellow green aqua pink wheat gray brown blue violet cyan
                         turquoise magenta salmon red))
       #t))

(define ((renderer2d->plot-data r) area)
  ((renderer2d-render-proc r) area)
  (void))

(define ((renderer3d->plot-data r) area)
  ((renderer3d-render-proc r) area)
  (void))

;; ===================================================================================================
;; Plotting

(define x-axis-data (renderer2d->plot-data (new.x-axis)))
(define y-axis-data (renderer2d->plot-data (new.y-axis)))

(defproc (plot [data ((is-a?/c 2d-plot-area%) . -> . void?)]
               [#:width width real? 400] [#:height height real? 400]
               [#:x-min x-min real? -5] [#:x-max x-max real? 5]
               [#:y-min y-min real? -5] [#:y-max y-max real? 5]
               [#:x-label x-label string? "X axis"] [#:y-label y-label string? "Y axis"]
               [#:title title string? ""]
               [#:fgcolor fgcolor (list/c byte? byte? byte?) '(0 0 0)]
               [#:bgcolor bgcolor (list/c byte? byte? byte?) '(255 255 255)]
               [#:lncolor lncolor (list/c byte? byte? byte?) '(255 0 0)]
               [#:out-file out-file (or/c path-string? output-port? #f) #f]
               ) (is-a?/c image-snip%)
  (define x-ticks ((new.plot-x-ticks) x-min x-max))
  (define y-ticks ((new.plot-y-ticks) y-min y-max))
  (define bounds-rect (vector (ivl x-min x-max) (ivl y-min y-max)))
  
  (parameterize ([new.plot-title       title]
                 [new.plot-x-label     x-label]
                 [new.plot-y-label     y-label]
                 [new.plot-foreground  fgcolor]
                 [new.plot-background  bgcolor])
    (define bm (make-bitmap (ceiling width) (ceiling height)))
    (define dc (make-object bitmap-dc% bm))
    (define area (make-object 2d-plot-area%
                   bounds-rect x-ticks x-ticks y-ticks y-ticks dc 0 0 width height))
    
    (define data+axes (mix x-axis-data y-axis-data data))
    
    (send area start-plot)
    (send area start-renderer bounds-rect)
    (data+axes area)
    (send area end-renderers)
    (send area end-plot)
    
    (when out-file (send bm save-file out-file 'png))
    
    (make-object image-snip% bm)))

(defproc (plot3d [data ((is-a?/c 3d-plot-area%) . -> . void?)]
                 [#:width width real? 400] [#:height height real? 400]
                 [#:x-min x-min real? -5] [#:x-max x-max real? 5]
                 [#:y-min y-min real? -5] [#:y-max y-max real? 5]
                 [#:z-min z-min real? -5] [#:z-max z-max real? 5]
                 [#:alt alt real? 30]
                 [#:az az real? 45]
                 [#:x-label x-label string? "X axis"]
                 [#:y-label y-label string? "Y axis"]
                 [#:z-label z-label string? "Z axis"]
                 [#:title title string? ""]
                 [#:fgcolor fgcolor (list/c byte? byte? byte?) '(0 0 0)]
                 [#:bgcolor bgcolor (list/c byte? byte? byte?) '(255 255 255)]
                 [#:lncolor lncolor (list/c byte? byte? byte?) '(255 0 0)]
                 [#:out-file out-file (or/c path-string? output-port? #f) #f]
                 ) (is-a?/c image-snip%)
  (define x-ticks ((new.plot-x-ticks) x-min x-max))
  (define y-ticks ((new.plot-y-ticks) y-min y-max))
  (define z-ticks ((new.plot-z-ticks) z-min z-max))
  (define bounds-rect (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max)))
  
  (parameterize ([new.plot-title       title]
                 [new.plot-x-label     x-label]
                 [new.plot-y-label     y-label]
                 [new.plot-z-label     z-label]
                 [new.plot-foreground  fgcolor]
                 [new.plot-background  bgcolor]
                 [new.plot3d-angle     az]
                 [new.plot3d-altitude  alt])
    (define bm (make-bitmap (ceiling width) (ceiling height)))
    (define dc (make-object bitmap-dc% bm))
    (define area (make-object 3d-plot-area%
                   bounds-rect x-ticks x-ticks y-ticks y-ticks z-ticks z-ticks dc 0 0 width height))
    
    (send area start-plot)
    (send area start-renderer bounds-rect)
    (data area)
    (send area end-renderers)
    (send area end-plot)
    
    (when out-file (send bm save-file out-file 'png))
    
    (make-object image-snip% bm)))

;; ===================================================================================================
;; Functions that generate "plot data"

(defproc (points [vecs (listof (vectorof real?))]
                 [#:sym sym (or/c char? string? exact-integer? symbol?) 'square]
                 [#:color color plot-color? 'black]
                 ) ((is-a?/c 2d-plot-area%) . -> . void?)
  (renderer2d->plot-data (new.points (map (λ (v) (vector-take v 2)) vecs)
                                     #:sym sym #:size 6 #:color color)))

(defproc (vector-field [f ((vector/c real? real?) . -> . (vector/c real? real?))]
                       [#:samples samples (and/c exact-integer? (>=/c 2)) 20]
                       [#:width width exact-positive-integer? 1]
                       [#:color color plot-color? 'red]
                       [#:style style (one-of/c 'scaled 'normalized 'real) 'scaled]
                       ) ((is-a?/c 2d-plot-area%) . -> . void?)
  (define scale (case style
                  [(scaled)      'auto]
                  [(normalized)  'normalized]
                  [(real)        1.0]))
  (renderer2d->plot-data
   (new.vector-field f #:samples samples #:line-width width #:color color #:scale scale)))

(defproc (error-bars [vecs (listof (vector/c real? real? real?))]
                     [#:color color plot-color? 'black]
                     ) ((is-a?/c 2d-plot-area%) . -> . void?)
  (renderer2d->plot-data (new.error-bars vecs #:color color #:alpha 1 #:width 4)))

(defproc (line [f (real? . -> . (or/c real? (vector/c real? real?)))]
               [#:samples samples (and/c exact-integer? (>=/c 2)) 150]
               [#:width width (>=/c 0) 1]
               [#:color color plot-color/c 'red]
               [#:mode mode (one-of/c 'standard 'parametric) 'standard]
               [#:mapping mapping (one-of/c 'cartesian 'polar) 'cartesian]
               [#:t-min t-min real? -5] [#:t-max t-max real? 5]
               ) ((is-a?/c 2d-plot-area%) . -> . void?)
  (renderer2d->plot-data (line-renderer f samples width color mode mapping t-min t-max)))

(defproc (contour [f (real? real? . -> . real?)]
                  [#:samples samples exact-nonnegative-integer? 50]
                  [#:width width (>=/c 0) 1]
                  [#:color color plot-color/c 'black]
                  [#:levels levels (or/c (and/c exact-integer? (>=/c 2)) (listof real?)) 10]
                  ) ((is-a?/c 2d-plot-area%) . -> . void?)
  (renderer2d->plot-data (contour-renderer f samples width color levels)))

(defproc (shade [f (real? real? . -> . real?)]
                [#:samples samples (and/c exact-integer? (>=/c 2)) 50]
                [#:levels levels (or/c (and/c exact-integer? (>=/c 2)) (listof real?)) 10]
                ) ((is-a?/c 2d-plot-area%) . -> . void?)
  (renderer2d->plot-data (shade-renderer f samples levels)))

(defproc (surface [f (real? real? . -> . real?)]
                  [#:samples samples (and/c exact-integer? (>=/c 2)) 50]
                  [#:width width (>=/c 0) 1]
                  [#:color color plot-color/c 'black]
                  ) ((is-a?/c 3d-plot-area%) . -> . void?)
  (renderer3d->plot-data (surface-renderer f samples width color)))
