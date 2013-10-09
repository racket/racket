#lang racket/base

(require racket/draw racket/contract racket/class
         unstable/contract
         pict
         unstable/parameter-group
         unstable/latent-contract/defthing
         "../common/contract.rkt"
         "../common/draw.rkt"
         "../common/parameters.rkt"
         "../common/plot-element.rkt"
         "../common/file-type.rkt"
         "../plot2d/plot-area.rkt"
         "plot2d-utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Plot to a given device context

(defproc (plot/dc [renderer-tree (treeof (or/c renderer2d? nonrenderer?))]
                  [dc (is-a?/c dc<%>)]
                  [x real?] [y real?] [width (>=/c 0)] [height (>=/c 0)]
                  [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
                  [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
                  [#:title title (or/c string? #f) (plot-title)]
                  [#:x-label x-label (or/c string? #f) (plot-x-label)]
                  [#:y-label y-label (or/c string? #f) (plot-y-label)]
                  [#:legend-anchor legend-anchor anchor/c (plot-legend-anchor)]) void?
  (define renderer-list (get-renderer-list renderer-tree))
  (define bounds-rect (get-bounds-rect renderer-list x-min x-max y-min y-max))
  (define-values (x-ticks x-far-ticks y-ticks y-far-ticks)
    (get-ticks renderer-list bounds-rect))
  
  (parameterize ([plot-title          title]
                 [plot-x-label        x-label]
                 [plot-y-label        y-label]
                 [plot-legend-anchor  legend-anchor])
    (define area (make-object 2d-plot-area%
                   bounds-rect x-ticks x-far-ticks y-ticks y-far-ticks dc x y width height))
    (plot-area area renderer-list)))

;; ===================================================================================================
;; Plot to a bitmap

(defproc (plot-bitmap [renderer-tree (treeof (or/c renderer2d? nonrenderer?))]
                      [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
                      [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
                      [#:width width exact-positive-integer? (plot-width)]
                      [#:height height exact-positive-integer? (plot-height)]
                      [#:title title (or/c string? #f) (plot-title)]
                      [#:x-label x-label (or/c string? #f) (plot-x-label)]
                      [#:y-label y-label (or/c string? #f) (plot-y-label)]
                      [#:legend-anchor legend-anchor anchor/c (plot-legend-anchor)]
                      ) (is-a?/c bitmap%)
  (define renderer-list (get-renderer-list renderer-tree))
  (define bounds-rect (get-bounds-rect renderer-list x-min x-max y-min y-max))
  (define-values (x-ticks x-far-ticks y-ticks y-far-ticks)
    (get-ticks renderer-list bounds-rect))
  ((if (plot-animating?) draw-bitmap draw-bitmap/supersampling)
   (λ (dc) 
     (plot/dc renderer-tree dc 0 0 width height
              #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
              #:title title #:x-label x-label #:y-label y-label #:legend-anchor legend-anchor))
   width height))

;; ===================================================================================================
;; Plot to a pict

(defproc (plot-pict [renderer-tree (treeof (or/c renderer2d? nonrenderer?))]
                    [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
                    [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
                    [#:width width exact-positive-integer? (plot-width)]
                    [#:height height exact-positive-integer? (plot-height)]
                    [#:title title (or/c string? #f) (plot-title)]
                    [#:x-label x-label (or/c string? #f) (plot-x-label)]
                    [#:y-label y-label (or/c string? #f) (plot-y-label)]
                    [#:legend-anchor legend-anchor anchor/c (plot-legend-anchor)]
                    ) pict?
  (define saved-values (plot-parameters))
  (dc (λ (dc x y)
        (parameterize/group
            ([plot-parameters  saved-values])
          (plot/dc renderer-tree dc x y width height
                   #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
                   #:title title #:x-label x-label #:y-label y-label #:legend-anchor legend-anchor)))
      width height))

;; ===================================================================================================
;; Plot to a file

(defproc (plot-file [renderer-tree (treeof (or/c renderer2d? nonrenderer?))]
                    [output (or/c path-string? output-port?)]
                    [kind (one-of/c 'auto 'png 'jpeg 'xmb 'xpm 'bmp 'ps 'pdf 'svg) 'auto]
                    [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
                    [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
                    [#:width width exact-positive-integer? (plot-width)]
                    [#:height height exact-positive-integer? (plot-height)]
                    [#:title title (or/c string? #f) (plot-title)]
                    [#:x-label x-label (or/c string? #f) (plot-x-label)]
                    [#:y-label y-label (or/c string? #f) (plot-y-label)]
                    [#:legend-anchor legend-anchor anchor/c (plot-legend-anchor)]) void?
  (define real-kind (if (eq? kind 'auto) (detect-image-file-type output) kind))
  (case real-kind
    [(png jpeg xbm xpm bmp)
     (define bm
       (plot-bitmap
        renderer-tree
        #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:width width #:height height
        #:title title #:x-label x-label #:y-label y-label #:legend-anchor legend-anchor))
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
     (plot/dc renderer-tree dc 0 0
              (inexact->exact (/ width x-scale)) (inexact->exact (/ height y-scale))
              #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
              #:title title #:x-label x-label #:y-label y-label #:legend-anchor legend-anchor)
     (send dc end-page)
     (send dc end-doc)])
  (void))
