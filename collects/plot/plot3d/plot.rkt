#lang racket/base

(require racket/draw racket/snip racket/match racket/list racket/class racket/contract
         slideshow/pict
         unstable/lazy-require
         (for-syntax racket/base)
         "../common/math.rkt"
         "../common/file-type.rkt"
         "../common/area.rkt"
         "../common/contract.rkt" "../common/contract-doc.rkt"
         "../common/parameters.rkt"
         "../common/deprecation-warning.rkt"
         "area.rkt"
         "renderer.rkt"
         "bounds.rkt")

;; Require lazily: without this, Racket complains while generating documentation:
;;   cannot instantiate `racket/gui/base' a second time in the same process
(lazy-require ["snip.rkt" (make-3d-plot-snip)]
              ["../common/gui.rkt" (make-snip-frame)])

(provide plot3d/dc plot3d plot3d-bitmap plot3d-pict plot3d-snip plot3d-frame plot3d-file)

;; ===================================================================================================
;; Plot to a given device context

(defproc (plot3d/dc [renderer-tree  (treeof renderer3d?)]
                    [dc  (is-a?/c dc<%>)]
                    [x real?] [y real?] [width (>=/c 0)] [height (>=/c 0)]
                    [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
                    [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
                    [#:z-min z-min (or/c real? #f) #f] [#:z-max z-max (or/c real? #f) #f]
                    [#:angle angle real? (plot3d-angle)] [#:altitude altitude real? (plot3d-altitude)]
                    [#:title title (or/c string? #f) (plot-title)]
                    [#:x-label x-label (or/c string? #f) (plot-x-label)]
                    [#:y-label y-label (or/c string? #f) (plot-y-label)]
                    [#:z-label z-label (or/c string? #f) (plot-z-label)]
                    [#:legend-anchor legend-anchor anchor/c (plot-legend-anchor)]) void?
  (define rs (filter (λ (renderer) (not (renderer3d-out-of-bounds?
                                         renderer x-min x-max y-min y-max z-min z-max)))
                     (flatten (list renderer-tree))))
  
  (define-values (px-min px-max py-min py-max pz-min pz-max)
    (renderer3d-bounds-fixpoint rs x-min x-max y-min y-max z-min z-max))
  
  (let ([x-min  (if x-min x-min px-min)]
        [x-max  (if x-max x-max px-max)]
        [y-min  (if y-min y-min py-min)]
        [y-max  (if y-max y-max py-max)]
        [z-min  (if z-min z-min pz-min)]
        [z-max  (if z-max z-max pz-max)])
    (when (or (not x-min) (not x-max) (x-min . >= . x-max))
      (error 'plot3d "could not determine x bounds; got: x-min = ~e, x-max = ~e" x-min x-max))
    (when (or (not y-min) (not y-max) (y-min . >= . y-max))
      (error 'plot3d "could not determine y bounds; got: y-min = ~e, y-max = ~e" y-min y-max))
    (when (or (not z-min) (not z-max) (z-min . >= . z-max))
      (error 'plot3d "could not determine z bounds; got: z-min = ~e, z-max = ~e" z-min z-max))
    
    (let ([x-min  (inexact->exact x-min)]
          [x-max  (inexact->exact x-max)]
          [y-min  (inexact->exact y-min)]
          [y-max  (inexact->exact y-max)]
          [z-min  (inexact->exact z-min)]
          [z-max  (inexact->exact z-max)])
      (define-values (all-x-ticks all-y-ticks all-z-ticks)
        (for/lists (all-x-ticks all-y-ticks all-z-ticks) ([r  (in-list rs)])
          ((renderer3d-ticks-fun r) x-min x-max y-min y-max z-min z-max)))
      
      (define x-ticks (remove-duplicates (append* all-x-ticks)))
      (define y-ticks (remove-duplicates (append* all-y-ticks)))
      (define z-ticks (remove-duplicates (append* all-z-ticks)))
      
      (parameterize ([plot3d-angle        angle]
                     [plot3d-altitude     altitude]
                     [plot-title          title]
                     [plot-x-label        x-label]
                     [plot-y-label        y-label]
                     [plot-z-label        z-label]
                     [plot-legend-anchor  legend-anchor])
        (define area (make-object 3d-plot-area%
                       x-ticks y-ticks z-ticks x-min x-max y-min y-max z-min z-max
                       dc x y width height))
        (send area start-plot)
        
        (define legend-entries
          (flatten (for/list ([renderer  (in-list rs)])
                     (match-define (renderer3d render-proc ticks-fun bounds-fun
                                               rx-min rx-max ry-min ry-max rz-min rz-max)
                       renderer)
                     (send area start-renderer rx-min rx-max ry-min ry-max rz-min rz-max)
                     (render-proc area))))
        
        (send area end-plot)
        
        (when (and (not (empty? legend-entries))
                   (or (not (plot-animating?))
                       (not (equal? (plot-legend-anchor) 'center))))
          (send area put-legend legend-entries))
        
        (when (plot-animating?) (send area put-angles))
        
        (send area restore-drawing-params)))))

;; ===================================================================================================
;; Plot to various other backends

;; Plot to a bitmap
(defproc (plot3d-bitmap [renderer-tree (treeof renderer3d?)]
                        [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
                        [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
                        [#:z-min z-min (or/c real? #f) #f] [#:z-max z-max (or/c real? #f) #f]
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
  (define bm (make-bitmap width height))
  (define dc (make-object bitmap-dc% bm))
  (plot3d/dc renderer-tree dc 0 0 width height
             #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:z-min z-min #:z-max z-max
             #:angle angle #:altitude altitude #:title title #:x-label x-label #:y-label y-label
             #:z-label z-label #:legend-anchor legend-anchor)
  bm)

(defproc (plot3d-pict [renderer-tree (treeof renderer3d?)]
                      [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
                      [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
                      [#:z-min z-min (or/c real? #f) #f] [#:z-max z-max (or/c real? #f) #f]
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
  (define foreground (plot-foreground))
  (define background (plot-background))
  (define foreground-alpha (plot-foreground-alpha))
  (define background-alpha (plot-background-alpha))
  (define font-size (plot-font-size))
  (define font-family (plot-font-family))
  (define line-width (plot-line-width))
  (define legend-box-alpha (plot-legend-box-alpha))
  (define tick-size (plot-tick-size))
  (define x-transform (plot-x-transform))
  (define y-transform (plot-y-transform))
  (define z-transform (plot-z-transform))
  (define x-ticks (plot-x-ticks))
  (define y-ticks (plot-y-ticks))
  (define z-ticks (plot-z-ticks))
  (define animating? (plot-animating?))
  (define samples (plot3d-samples))
  (define ambient-light (plot3d-ambient-light))
  (define diffuse-light? (plot3d-diffuse-light?))
  (define specular-light? (plot3d-specular-light?))
  
  (dc (λ (dc x y)
        (parameterize ([plot-foreground         foreground]
                       [plot-background         background]
                       [plot-foreground-alpha   foreground-alpha]
                       [plot-background-alpha   background-alpha]
                       [plot-font-size          font-size]
                       [plot-font-family        font-family]
                       [plot-line-width         line-width]
                       [plot-legend-box-alpha   legend-box-alpha]
                       [plot-tick-size          tick-size]
                       [plot-x-transform        x-transform]
                       [plot-y-transform        y-transform]
                       [plot-z-transform        z-transform]
                       [plot-x-ticks            x-ticks]
                       [plot-y-ticks            y-ticks]
                       [plot-z-ticks            z-ticks]
                       [plot-animating?         animating?]
                       [plot3d-samples          samples]
                       [plot3d-ambient-light    ambient-light]
                       [plot3d-diffuse-light?   diffuse-light?]
                       [plot3d-specular-light?  specular-light?])
          (plot3d/dc
           renderer-tree dc x y width height
           #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:z-min z-min #:z-max z-max
           #:angle angle #:altitude altitude #:title title #:x-label x-label #:y-label y-label
           #:z-label z-label #:legend-anchor legend-anchor)))
      width height))

;; Plot to a snip
(defproc (plot3d-snip [renderer-tree (treeof renderer3d?)]
                      [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
                      [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
                      [#:z-min z-min (or/c real? #f) #f] [#:z-max z-max (or/c real? #f) #f]
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
  (make-3d-plot-snip
   (λ (angle altitude anim?)
     (parameterize ([plot-animating?  (if anim? #t (plot-animating?))])
       (plot3d-bitmap
        renderer-tree
        #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:z-min z-min #:z-max z-max
        #:width width #:height height #:angle angle #:altitude altitude #:title title
        #:x-label x-label #:y-label y-label #:z-label z-label #:legend-anchor legend-anchor)))
   angle altitude))

;; Plot to a frame
(defproc (plot3d-frame [renderer-tree (treeof renderer3d?)]
                       [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
                       [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
                       [#:z-min z-min (or/c real? #f) #f] [#:z-max z-max (or/c real? #f) #f]
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
(defproc (plot3d-file [renderer-tree (treeof renderer3d?)]
                      [output (or/c path-string? output-port?)]
                      [kind (one-of/c 'auto 'png 'jpeg 'xmb 'xpm 'bmp 'ps 'pdf 'svg) 'auto]
                      [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
                      [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
                      [#:z-min z-min (or/c real? #f) #f] [#:z-max z-max (or/c real? #f) #f]
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
(defproc (plot3d [renderer-tree (treeof renderer3d?)]
                 [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
                 [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
                 [#:z-min z-min (or/c real? #f) #f] [#:z-max z-max (or/c real? #f) #f]
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
                 [#:fgcolor fgcolor plot-color/c #f]
                 [#:bgcolor bgcolor plot-color/c #f]
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
    
    (cond [(plot-new-window?)  (define frame (call plot3d-frame))
                               (send frame show #t)
                               (void)]
          [else  (call plot3d-snip)])))
