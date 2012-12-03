#lang racket/base

;; Procedures that plot 2D renderers.

(require racket/draw racket/snip racket/contract racket/list racket/class racket/match
         unstable/contract
         slideshow/pict
         unstable/parameter-group
         racket/lazy-require
         unstable/latent-contract/defthing
         "../common/contract.rkt"
         "../common/math.rkt"
         "../common/draw.rkt"
         "../common/parameters.rkt"
         "../common/plot-element.rkt"
         "../common/file-type.rkt"
         "../common/deprecation-warning.rkt"
         "../common/format.rkt"
         "plot-area.rkt")

;; Require lazily: without this, Racket complains while generating documentation:
;;   cannot instantiate `racket/gui/base' a second time in the same process
(lazy-require ["snip.rkt" (make-2d-plot-snip)]
              ["../common/gui.rkt" (make-snip-frame with-new-eventspace)])

(provide (except-out (all-defined-out) get-renderer-list get-bounds-rect get-ticks plot-dc))

;; ===================================================================================================
;; Plot to a given device context

(define (get-renderer-list renderer-tree)
  (for/list ([r  (flatten (list renderer-tree))])
    (match r
      [(nonrenderer bounds-rect bounds-fun ticks-fun)
       (renderer2d bounds-rect bounds-fun ticks-fun #f)]
      [_  r])))

(define (get-bounds-rect renderer-list x-min x-max y-min y-max)
  (define given-bounds-rect (vector (ivl x-min x-max) (ivl y-min y-max)))
  (define plot-bounds-rect (bounds-fixpoint renderer-list given-bounds-rect))
  (when (or (not (rect-rational? plot-bounds-rect))
            (rect-zero-area? plot-bounds-rect))
    (match-define (vector x-ivl y-ivl) plot-bounds-rect)
    (error 'plot "could not determine sensible plot bounds; got x ∈ ~a, y ∈ ~a"
           (ivl->plot-label x-ivl) (ivl->plot-label y-ivl)))
  (rect-inexact->exact plot-bounds-rect))

(define (get-ticks renderer-list bounds-rect)
  (define-values (all-x-ticks all-x-far-ticks all-y-ticks all-y-far-ticks)
    (for/lists (all-x-ticks all-x-far-ticks all-y-ticks all-y-far-ticks
                            ) ([r  (in-list renderer-list)])
      (define ticks-fun (plot-element-ticks-fun r))
      (cond [ticks-fun  (ticks-fun bounds-rect)]
            [else       (values empty empty empty empty)])))
  (values (remove-duplicates (append* all-x-ticks))
          (remove-duplicates (append* all-x-far-ticks))
          (remove-duplicates (append* all-y-ticks))
          (remove-duplicates (append* all-y-far-ticks))))

(define (plot-dc renderer-list bounds-rect x-ticks x-far-ticks y-ticks y-far-ticks
                 dc x y width height)
  (define area (make-object 2d-plot-area%
                 bounds-rect x-ticks x-far-ticks y-ticks y-far-ticks dc x y width height))
  (send area start-plot)
  
  (define legend-entries
    (flatten (for/list ([rend  (in-list renderer-list)])
               (match-define (renderer2d rend-bounds-rect _bf _tf render-proc) rend)
               (send area start-renderer (if rend-bounds-rect
                                             (rect-inexact->exact rend-bounds-rect)
                                             (unknown-rect 2)))
               (if render-proc (render-proc area) empty))))
  
  (send area end-renderers)
  
  (when (not (empty? legend-entries))
    (send area draw-legend legend-entries))
  
  (send area end-plot))

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
    (plot-dc renderer-list bounds-rect x-ticks x-far-ticks y-ticks y-far-ticks
             dc x y width height)))

;; ===================================================================================================
;; Plot to various other backends

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
             
             (send area start-plot)
             
             (define legend-entries
               (flatten (for/list ([rend  (in-list renderer-list)])
                          (match-define (renderer2d rend-bounds-rect _bf _tf render-proc) rend)
                          (send area start-renderer (if rend-bounds-rect
                                                        (rect-inexact->exact rend-bounds-rect)
                                                        (unknown-rect 2)))
                          (if render-proc (render-proc area) empty))))
             
             (send area end-renderers)
             
             (when (not (empty? legend-entries))
               (send area draw-legend legend-entries))
             
             (send area end-plot))
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
