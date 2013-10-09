#lang racket/base

(require racket/snip racket/contract racket/class racket/match
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
         plot/private/plot2d/plot-area
         plot/private/no-gui/plot2d
         plot/private/no-gui/plot2d-utils)

;; Require lazily, in case someone wants to just (require plot) in a headless setup
(lazy-require ["snip2d.rkt" (make-2d-plot-snip)]
              ["gui.rkt" (make-snip-frame with-new-eventspace)])

(provide plot-snip plot-frame plot)

;; ===================================================================================================
;; Plot to a snip

(defproc (plot-snip [renderer-tree (treeof (or/c renderer2d? nonrenderer?))]
                    [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
                    [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
                    [#:width width exact-positive-integer? (plot-width)]
                    [#:height height exact-positive-integer? (plot-height)]
                    [#:title title (or/c string? #f) (plot-title)]
                    [#:x-label x-label (or/c string? #f) (plot-x-label)]
                    [#:y-label y-label (or/c string? #f) (plot-y-label)]
                    [#:legend-anchor legend-anchor anchor/c (plot-legend-anchor)]
                    ) (is-a?/c image-snip%)
  (parameterize ([plot-title          title]
                 [plot-x-label        x-label]
                 [plot-y-label        y-label]
                 [plot-legend-anchor  legend-anchor])
    (define saved-plot-parameters (plot-parameters))
    (define renderer-list (get-renderer-list renderer-tree))
    (define bounds-rect (get-bounds-rect renderer-list x-min x-max y-min y-max))
    
    (define (make-bm anim? bounds-rect width height)
      (define area #f)
      (define bm
        (parameterize/group ([plot-parameters  saved-plot-parameters]
                             [plot-animating?  (if anim? #t (plot-animating?))])
          ((if (plot-animating?) draw-bitmap draw-bitmap/supersampling)
           (λ (dc)
             (define-values (x-ticks x-far-ticks y-ticks y-far-ticks)
               (get-ticks renderer-list bounds-rect))
             
             (set! area (make-object 2d-plot-area%
                          bounds-rect x-ticks x-far-ticks y-ticks y-far-ticks
                          dc 0 0 width height))
             
             (plot-area area renderer-list))
           width height)))
      
      (define (area-bounds->plot-bounds rect)
        (match-define (vector (ivl area-x-min area-x-max) (ivl area-y-min area-y-max)) rect)
        (match-define (vector x-min y-min) (send area dc->plot (vector area-x-min area-y-min)))
        (match-define (vector x-max y-max) (send area dc->plot (vector area-x-max area-y-max)))
        (vector (ivl x-min x-max) (ivl y-min y-max)))
      
      (values bm (send area get-area-bounds-rect) area-bounds->plot-bounds))
    
    (define-values (bm area-bounds-rect area-bounds->plot-bounds)
      (make-bm #f bounds-rect width height))
    
    (make-2d-plot-snip
     bm saved-plot-parameters
     make-bm bounds-rect area-bounds-rect area-bounds->plot-bounds width height)))

;; ===================================================================================================
;; Plot to a frame

(defproc (plot-frame [renderer-tree (treeof (or/c renderer2d? nonrenderer?))]
                     [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
                     [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
                     [#:width width exact-positive-integer? (plot-width)]
                     [#:height height exact-positive-integer? (plot-height)]
                     [#:title title (or/c string? #f) (plot-title)]
                     [#:x-label x-label (or/c string? #f) (plot-x-label)]
                     [#:y-label y-label (or/c string? #f) (plot-y-label)]
                     [#:legend-anchor legend-anchor anchor/c (plot-legend-anchor)]
                     ) (is-a?/c object%)
  (define snip
    (plot-snip
     renderer-tree
     #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:width width #:height height
     #:title title #:x-label x-label #:y-label y-label #:legend-anchor legend-anchor))
  (make-snip-frame snip width height (if title (format "Plot: ~a" title) "Plot")))

;; ===================================================================================================
;; Plot to a frame or a snip, depending on (plot-new-window?)

(defproc (plot [renderer-tree (treeof (or/c renderer2d? nonrenderer?))]
               [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
               [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
               [#:width width exact-positive-integer? (plot-width)]
               [#:height height exact-positive-integer? (plot-height)]
               [#:title title (or/c string? #f) (plot-title)]
               [#:x-label x-label (or/c string? #f) (plot-x-label)]
               [#:y-label y-label (or/c string? #f) (plot-y-label)]
               [#:legend-anchor legend-anchor anchor/c (plot-legend-anchor)]
               [#:out-file out-file (or/c path-string? output-port? #f) #f]
               [#:out-kind out-kind (one-of/c 'auto 'png 'jpeg 'xmb 'xpm 'bmp 'ps 'pdf 'svg) 'auto]
               [#:fgcolor fgcolor plot-color/c #f] [#:bgcolor bgcolor plot-color/c #f]
               [#:lncolor lncolor plot-color/c #f]  ; unused
               ) (or/c (is-a?/c snip%) void?)
  (when fgcolor
    (deprecation-warning "the plot #:fgcolor keyword argument" "plot-foreground"))
  (when bgcolor
    (deprecation-warning "the plot #:bgcolor keyword argument" "plot-background"))
  (when lncolor
    (deprecation-warning "the plot #:lncolor keyword argument"))
  
  (define (call f . args)
    (apply f renderer-tree args
           #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:width width #:height height
           #:title title #:x-label x-label #:y-label y-label #:legend-anchor legend-anchor))
  
  (parameterize ([plot-foreground  (if fgcolor fgcolor (plot-foreground))]
                 [plot-background  (if bgcolor bgcolor (plot-background))])
    (when out-file
      (call plot-file out-file out-kind))
    
    (cond [(plot-new-window?)  (define frame (with-new-eventspace (λ () (call plot-frame))))
                               (send frame show #t)
                               (void)]
          [else  (call plot-snip)])))
