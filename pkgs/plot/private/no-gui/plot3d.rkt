#lang racket/base

(require racket/draw racket/class racket/contract
         unstable/contract
         pict
         unstable/parameter-group
         unstable/latent-contract/defthing
         "../common/contract.rkt"
         "../common/draw.rkt"
         "../common/parameters.rkt"
         "../common/plot-element.rkt"
         "../common/file-type.rkt"
         "../plot3d/plot-area.rkt"
         "plot3d-utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Plot to a given device context

(defproc (plot3d/dc [renderer-tree  (treeof (or/c renderer3d? nonrenderer?))]
                    [dc  (is-a?/c dc<%>)]
                    [x real?] [y real?] [width (>=/c 0)] [height (>=/c 0)]
                    [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
                    [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
                    [#:z-min z-min (or/c rational? #f) #f] [#:z-max z-max (or/c rational? #f) #f]
                    [#:angle angle real? (plot3d-angle)] [#:altitude altitude real? (plot3d-altitude)]
                    [#:title title (or/c string? #f) (plot-title)]
                    [#:x-label x-label (or/c string? #f) (plot-x-label)]
                    [#:y-label y-label (or/c string? #f) (plot-y-label)]
                    [#:z-label z-label (or/c string? #f) (plot-z-label)]
                    [#:legend-anchor legend-anchor anchor/c (plot-legend-anchor)]) void?
  (define renderer-list (get-renderer-list renderer-tree))
  (define bounds-rect (get-bounds-rect renderer-list x-min x-max y-min y-max z-min z-max))
  (define-values (x-ticks x-far-ticks y-ticks y-far-ticks z-ticks z-far-ticks)
    (get-ticks renderer-list bounds-rect))
  
  (parameterize ([plot3d-angle        angle]
                 [plot3d-altitude     altitude]
                 [plot-title          title]
                 [plot-x-label        x-label]
                 [plot-y-label        y-label]
                 [plot-z-label        z-label]
                 [plot-legend-anchor  legend-anchor])
    (define area (make-object 3d-plot-area%
                   bounds-rect x-ticks x-far-ticks y-ticks y-far-ticks z-ticks z-far-ticks
                   dc x y width height))
    (plot-area area renderer-list)))

;; ===================================================================================================
;; Plot to a bitmap

(defproc (plot3d-bitmap [renderer-tree (treeof (or/c renderer3d? nonrenderer?))]
                        [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
                        [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
                        [#:z-min z-min (or/c rational? #f) #f] [#:z-max z-max (or/c rational? #f) #f]
                        [#:width width exact-positive-integer? (plot-width)]
                        [#:height height exact-positive-integer? (plot-height)]
                        [#:angle angle real? (plot3d-angle)]
                        [#:altitude altitude real? (plot3d-altitude)]
                        [#:title title (or/c string? #f) (plot-title)]
                        [#:x-label x-label (or/c string? #f) (plot-x-label)]
                        [#:y-label y-label (or/c string? #f) (plot-y-label)]
                        [#:z-label z-label (or/c string? #f) (plot-z-label)]
                        [#:legend-anchor legend-anchor anchor/c (plot-legend-anchor)]
                        ) (is-a?/c bitmap%)
  ((if (plot-animating?) draw-bitmap draw-bitmap/supersampling)
   (λ (dc)
     (plot3d/dc renderer-tree dc 0 0 width height
                #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:z-min z-min #:z-max z-max
                #:angle angle #:altitude altitude #:title title #:x-label x-label #:y-label y-label
                #:z-label z-label #:legend-anchor legend-anchor))
   width height))

;; ===================================================================================================
;; Plot to a pict

(defproc (plot3d-pict [renderer-tree (treeof (or/c renderer3d? nonrenderer?))]
                      [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
                      [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
                      [#:z-min z-min (or/c rational? #f) #f] [#:z-max z-max (or/c rational? #f) #f]
                      [#:width width exact-positive-integer? (plot-width)]
                      [#:height height exact-positive-integer? (plot-height)]
                      [#:angle angle real? (plot3d-angle)]
                      [#:altitude altitude real? (plot3d-altitude)]
                      [#:title title (or/c string? #f) (plot-title)]
                      [#:x-label x-label (or/c string? #f) (plot-x-label)]
                      [#:y-label y-label (or/c string? #f) (plot-y-label)]
                      [#:z-label z-label (or/c string? #f) (plot-z-label)]
                      [#:legend-anchor legend-anchor anchor/c (plot-legend-anchor)]
                      ) pict?
  (define saved-plot-parameters (plot-parameters))
  (dc (λ (dc x y)
        (parameterize/group ([plot-parameters  saved-plot-parameters])
          (plot3d/dc renderer-tree dc x y width height
                     #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:z-min z-min
                     #:z-max z-max #:angle angle #:altitude altitude #:title title #:x-label x-label
                     #:y-label y-label #:z-label z-label #:legend-anchor legend-anchor)))
      width height))

;; ===================================================================================================
;; Plot to any supported kind of file

(defproc (plot3d-file [renderer-tree (treeof (or/c renderer3d? nonrenderer?))]
                      [output (or/c path-string? output-port?)]
                      [kind (one-of/c 'auto 'png 'jpeg 'xmb 'xpm 'bmp 'ps 'pdf 'svg) 'auto]
                      [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
                      [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
                      [#:z-min z-min (or/c rational? #f) #f] [#:z-max z-max (or/c rational? #f) #f]
                      [#:width width exact-positive-integer? (plot-width)]
                      [#:height height exact-positive-integer? (plot-height)]
                      [#:angle angle real? (plot3d-angle)]
                      [#:altitude altitude real? (plot3d-altitude)]
                      [#:title title (or/c string? #f) (plot-title)]
                      [#:x-label x-label (or/c string? #f) (plot-x-label)]
                      [#:y-label y-label (or/c string? #f) (plot-y-label)]
                      [#:z-label z-label (or/c string? #f) (plot-z-label)]
                      [#:legend-anchor legend-anchor anchor/c (plot-legend-anchor)]) void?
  (define real-kind (if (eq? kind 'auto) (detect-image-file-type output) kind))
  (case real-kind
    [(png jpeg xbm xpm bmp)
     (define bm
       (plot3d-bitmap
        renderer-tree
        #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:z-min z-min #:z-max z-max
        #:width width #:height height #:angle angle #:altitude altitude #:title title
        #:x-label x-label #:y-label y-label #:z-label z-label #:legend-anchor legend-anchor))
     (send bm save-file output real-kind (plot-jpeg-quality))]
    [(ps pdf svg)
     (define dc
       (case real-kind
         [(ps)  (new post-script-dc%
                     [interactive (plot-ps/pdf-interactive?)] [parent #f] [use-paper-bbox #f]
                     [as-eps #t] [width width] [height height] [output output])]
         [(pdf)  (new pdf-dc%
                      [interactive (plot-ps/pdf-interactive?)] [parent #f] [use-paper-bbox #f]
                      [width width] [height height] [output output])]
         [(svg)  (new svg-dc%
                      [width width] [height height] [output output] [exists 'truncate/replace])]))
     (define-values (x-scale y-scale) (send dc get-device-scale))
     (send dc start-doc "Rendering plot")
     (send dc start-page)
     (plot3d/dc renderer-tree dc 0 0
                (inexact->exact (/ width x-scale)) (inexact->exact (/ height y-scale))
                #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:z-min z-min #:z-max z-max
                #:angle angle #:altitude altitude #:title title #:x-label x-label #:y-label y-label
                #:z-label z-label #:legend-anchor legend-anchor)
     (send dc end-page)
     (send dc end-doc)])
  (void))
