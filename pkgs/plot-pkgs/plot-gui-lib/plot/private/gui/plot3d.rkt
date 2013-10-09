#lang racket/base

(require racket/snip racket/match racket/list racket/class racket/contract
         unstable/contract
         unstable/parameter-group
         racket/lazy-require
         unstable/latent-contract/defthing
         plot/private/common/contract
         plot/private/common/math
         plot/private/common/draw
         plot/private/common/parameters
         plot/private/common/plot-element
         plot/private/common/deprecation-warning
         plot/private/plot3d/plot-area
         plot/private/no-gui/plot3d
         plot/private/no-gui/plot3d-utils)

;; Require lazily, in case someone wants to just (require plot) in a headless setup
(lazy-require ["snip3d.rkt" (make-3d-plot-snip)]
              ["gui.rkt" (make-snip-frame with-new-eventspace)])

(provide plot3d-snip plot3d-frame plot3d)

;; ===================================================================================================
;; Plot to a snip

(defproc (plot3d-snip [renderer-tree (treeof (or/c renderer3d? nonrenderer?))]
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
                      ) (is-a?/c image-snip%)
  (parameterize ([plot-title          title]
                 [plot-x-label        x-label]
                 [plot-y-label        y-label]
                 [plot-z-label        z-label]
                 [plot-legend-anchor  legend-anchor])
    (define saved-plot-parameters (plot-parameters))
    (define renderer-list (get-renderer-list renderer-tree))
    (define bounds-rect (get-bounds-rect renderer-list x-min x-max y-min y-max z-min z-max))
    (define-values (x-ticks x-far-ticks y-ticks y-far-ticks z-ticks z-far-ticks)
      (get-ticks renderer-list bounds-rect))
    
    (define render-list-hash (make-hash))
    (define legend-entries-hash (make-hash))
    
    (define (make-bm anim? angle altitude width height)
      (parameterize/group ([plot-parameters  saved-plot-parameters]
                           [plot-animating?  (if anim? #t (plot-animating?))]
                           [plot3d-angle     angle]
                           [plot3d-altitude  altitude])
        ((if (plot-animating?) draw-bitmap draw-bitmap/supersampling)
         (λ (dc)
           (define area (make-object 3d-plot-area%
                          bounds-rect x-ticks x-far-ticks y-ticks y-far-ticks z-ticks z-far-ticks
                          dc 0 0 width height))
           (send area start-plot)
           
           (cond [(not (hash-ref render-list-hash (plot-animating?) #f))
                  (hash-set!
                   legend-entries-hash (plot-animating?)
                   (flatten (for/list ([rend  (in-list renderer-list)])
                              (match-define (renderer3d rend-bounds-rect _bf _tf render-proc) rend)
                              (send area start-renderer (if rend-bounds-rect
                                                            (rect-inexact->exact rend-bounds-rect)
                                                            (unknown-rect 3)))
                              (if render-proc (render-proc area) empty))))
                  
                  (hash-set! render-list-hash (plot-animating?) (send area get-render-list))]
                 [else
                  (send area put-render-list (hash-ref render-list-hash (plot-animating?)))])
           
           (send area end-renderers)
           
           (define legend-entries (hash-ref legend-entries-hash (plot-animating?) #f))
           (when (not (empty? legend-entries))
             (send area draw-legend legend-entries))
           
           (send area end-plot))
         width height)))
    
    (make-3d-plot-snip
     (make-bm #f angle altitude width height) saved-plot-parameters
     make-bm angle altitude width height)))

;; ===================================================================================================
;; Plot to a frame

(defproc (plot3d-frame [renderer-tree (treeof (or/c renderer3d? nonrenderer?))]
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
                       ) (is-a?/c object%)
  (define snip
    (plot3d-snip
     renderer-tree
     #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:z-min z-min #:z-max z-max
     #:width width #:height height #:angle angle #:altitude altitude #:title title
     #:x-label x-label #:y-label y-label #:z-label z-label #:legend-anchor legend-anchor))
  (make-snip-frame snip width height (if title (format "Plot: ~a" title) "Plot")))

;; ===================================================================================================
;; Plot to a frame or a snip, depending on the value of plot-new-window?

(defproc (plot3d [renderer-tree (treeof (or/c renderer3d? nonrenderer?))]
                 [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
                 [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
                 [#:z-min z-min (or/c rational? #f) #f] [#:z-max z-max (or/c rational? #f) #f]
                 [#:width width exact-positive-integer? (plot-width)]
                 [#:height height exact-positive-integer? (plot-height)]
                 [#:angle angle real? #f] [#:altitude altitude real? #f]
                 [#:az az real? #f] [#:alt alt real? #f]  ; backward-compatible aliases
                 [#:title title (or/c string? #f) (plot-title)]
                 [#:x-label x-label (or/c string? #f) (plot-x-label)]
                 [#:y-label y-label (or/c string? #f) (plot-y-label)]
                 [#:z-label z-label (or/c string? #f) (plot-z-label)]
                 [#:legend-anchor legend-anchor anchor/c (plot-legend-anchor)]
                 [#:out-file out-file (or/c path-string? output-port? #f) #f]
                 [#:out-kind out-kind (one-of/c 'auto 'png 'jpeg 'xmb 'xpm 'bmp 'ps 'pdf 'svg) 'auto]
                 [#:fgcolor fgcolor plot-color/c #f] [#:bgcolor bgcolor plot-color/c #f]
                 [#:lncolor lncolor plot-color/c #f]  ; unused
                 ) (or/c (is-a?/c snip%) void?)
  (when fgcolor
    (deprecation-warning "the plot3d #:fgcolor keyword argument" "plot-foreground"))
  (when bgcolor
    (deprecation-warning "the plot3d #:bgcolor keyword argument" "plot-background"))
  (when lncolor
    (deprecation-warning "the plot3d #:lncolor keyword argument"))
  (when az
    (deprecation-warning "the plot3d #:az keyword argument" "#:angle"))
  (when alt
    (deprecation-warning "the plot3d #:alt keyword argument" "#:altitude"))
  
  (define (call f . args)
    (apply f renderer-tree args
           #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:z-min z-min #:z-max z-max
           #:width width #:height height #:title title
           #:angle (or angle az (plot3d-angle)) #:altitude (or altitude alt (plot3d-altitude))
           #:x-label x-label #:y-label y-label #:z-label z-label #:legend-anchor legend-anchor))
  
  (parameterize ([plot-foreground  (if fgcolor fgcolor (plot-foreground))]
                 [plot-background  (if bgcolor bgcolor (plot-background))])
    (when out-file
      (call plot3d-file out-file out-kind))
    
    (cond [(plot-new-window?)  (define frame (with-new-eventspace (λ () (call plot3d-frame))))
                               (send frame show #t)
                               (void)]
          [else  (call plot3d-snip)])))
