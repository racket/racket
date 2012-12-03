#lang racket/base

;; Procedures that plot 3D renderers.

(require racket/draw racket/snip racket/match racket/list racket/class racket/contract
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
(lazy-require ["snip.rkt" (make-3d-plot-snip)]
              ["../common/gui.rkt" (make-snip-frame with-new-eventspace)])

(provide (except-out (all-defined-out) get-renderer-list get-bounds-rect get-ticks plot3d-dc))

;; ===================================================================================================
;; Plot to a given device context

(define (get-renderer-list renderer-tree)
  (for/list ([r  (flatten (list renderer-tree))])
    (match r
      [(nonrenderer bounds-rect bounds-fun ticks-fun)
       (renderer3d bounds-rect bounds-fun ticks-fun #f)]
      [_  r])))

(define (get-bounds-rect renderer-list x-min x-max y-min y-max z-min z-max)
  (define given-bounds-rect (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max)))
  (define plot-bounds-rect (bounds-fixpoint renderer-list given-bounds-rect))
  (when (or (not (rect-rational? plot-bounds-rect))
            (rect-zero-area? plot-bounds-rect))
    (match-define (vector x-ivl y-ivl z-ivl) plot-bounds-rect)
    (error 'plot "could not determine sensible plot bounds; got x ∈ ~a, y ∈ ~a, z ∈ ~a"
           (ivl->plot-label x-ivl) (ivl->plot-label y-ivl) (ivl->plot-label z-ivl)))
  (rect-inexact->exact plot-bounds-rect))

(define (get-ticks renderer-list bounds-rect)
  (define-values (all-x-ticks all-x-far-ticks all-y-ticks all-y-far-ticks all-z-ticks all-z-far-ticks)
    (for/lists (all-x-ticks
                all-x-far-ticks
                all-y-ticks
                all-y-far-ticks
                all-z-ticks
                all-z-far-ticks) ([r  (in-list renderer-list)])
      (define ticks-fun (plot-element-ticks-fun r))
      (cond [ticks-fun  (ticks-fun bounds-rect)]
            [else       (values empty empty empty empty empty empty)])))
  (values (remove-duplicates (append* all-x-ticks))
          (remove-duplicates (append* all-x-far-ticks))
          (remove-duplicates (append* all-y-ticks))
          (remove-duplicates (append* all-y-far-ticks))
          (remove-duplicates (append* all-z-ticks))
          (remove-duplicates (append* all-z-far-ticks))))

(define (plot3d-dc renderer-list bounds-rect
                   x-ticks x-far-ticks y-ticks y-far-ticks z-ticks z-far-ticks
                   dc x y width height)
  (define area (make-object 3d-plot-area%
                 bounds-rect x-ticks x-far-ticks y-ticks y-far-ticks z-ticks z-far-ticks
                 dc x y width height))
  (send area start-plot)
  
  (define legend-entries
    (flatten (for/list ([rend  (in-list renderer-list)])
               (match-define (renderer3d rend-bounds-rect _bf _tf render-proc) rend)
               (send area start-renderer (if rend-bounds-rect
                                             (rect-inexact->exact rend-bounds-rect)
                                             (unknown-rect 3)))
               (if render-proc (render-proc area) empty))))
  
  (send area end-renderers)
  
  (when (not (empty? legend-entries))
    (send area draw-legend legend-entries))
  
  (send area end-plot))


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
    (plot3d-dc renderer-list bounds-rect
               x-ticks x-far-ticks y-ticks y-far-ticks z-ticks z-far-ticks
               dc x y width height)))

;; ===================================================================================================
;; Plot to various other backends

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
