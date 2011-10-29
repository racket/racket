#lang racket/base

(require racket/draw racket/snip racket/match racket/list racket/class racket/contract
         slideshow/pict
         unstable/parameter-group
         unstable/lazy-require
         (for-syntax racket/base)
         "../common/math.rkt"
         "../common/file-type.rkt"
         "../common/area.rkt"
         "../common/contract.rkt"
         "../common/contract-doc.rkt"
         "../common/parameters.rkt"
         "../common/deprecation-warning.rkt"
         "../common/plot-element.rkt"
         "../common/non-renderer.rkt"
         "../common/utils.rkt"
         "area.rkt")

;; Require lazily: without this, Racket complains while generating documentation:
;;   cannot instantiate `racket/gui/base' a second time in the same process
(lazy-require ["snip.rkt" (make-3d-plot-snip)]
              ["../common/gui.rkt" (make-snip-frame)])

(provide plot3d/dc plot3d plot3d-bitmap plot3d-pict plot3d-snip plot3d-frame plot3d-file)

;; ===================================================================================================
;; Plot to a given device context

(defproc (plot3d/dc [renderer-tree  (treeof (or/c renderer3d? non-renderer?))]
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
  (define given-bounds-rect (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max)))
  (define rs (for/list ([r  (flatten (list renderer-tree))])
               (match r
                 [(non-renderer bounds-rect bounds-fun ticks-fun)
                  (renderer3d bounds-rect bounds-fun ticks-fun #f)]
                 [_  r])))
  
  (define plot-bounds-rect (bounds-fixpoint rs given-bounds-rect))
  
  (when (or (not (rect-regular? plot-bounds-rect))
            (rect-zero-area? plot-bounds-rect))
    (match-define (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max)) plot-bounds-rect)
    (error 'plot "~a; determined x ∈ [~e,~e], y ∈ [~e,~e], z ∈ [~e,~e]"
           "could not determine sensible plot bounds" x-min x-max y-min y-max z-min z-max))
  
  (define bounds-rect (rect-inexact->exact plot-bounds-rect))
  
  (define-values (all-x-ticks all-x-far-ticks all-y-ticks all-y-far-ticks all-z-ticks all-z-far-ticks)
    (for/lists (all-x-ticks
                all-x-far-ticks
                all-y-ticks
                all-y-far-ticks
                all-z-ticks
                all-z-far-ticks) ([r  (in-list rs)])
      (define ticks-fun (plot-element-ticks-fun r))
      (cond [ticks-fun  (ticks-fun bounds-rect)]
            [else  (values empty empty empty empty empty empty)])))
  
  (define x-ticks (remove-duplicates (append* all-x-ticks)))
  (define y-ticks (remove-duplicates (append* all-y-ticks)))
  (define z-ticks (remove-duplicates (append* all-z-ticks)))
  
  (define x-far-ticks (remove-duplicates (append* all-x-far-ticks)))
  (define y-far-ticks (remove-duplicates (append* all-y-far-ticks)))
  (define z-far-ticks (remove-duplicates (append* all-z-far-ticks)))
  
  (parameterize ([plot3d-angle        angle]
                 [plot3d-altitude     altitude]
                 [plot-title          title]
                 [plot-x-label        x-label]
                 [plot-y-label        y-label]
                 [plot-z-label        z-label]
                 [plot-legend-anchor  legend-anchor])
    (match-define (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max)) bounds-rect)
    (define area (make-object 3d-plot-area%
                   x-ticks x-far-ticks y-ticks y-far-ticks z-ticks z-far-ticks
                   x-min x-max y-min y-max z-min z-max
                   dc x y width height))
    (send area start-plot)
    
    (define legend-entries
      (flatten (for/list ([rend  (in-list rs)])
                 (match-define (renderer3d rend-bounds-rect _bf _tf render-proc) rend)
                 (match-define (vector (ivl rx-min rx-max) (ivl ry-min ry-max) (ivl rz-min rz-max))
                   (if rend-bounds-rect rend-bounds-rect (empty-rect 3)))
                 (send area start-renderer rx-min rx-max ry-min ry-max rz-min rz-max)
                 (if render-proc (render-proc area) empty))))
    
    (send area end-plot)
    
    (when (and (not (empty? legend-entries))
               (or (not (plot-animating?))
                   (not (equal? (plot-legend-anchor) 'center))))
      (send area put-legend legend-entries))
    
    (when (plot-animating?) (send area put-angles))
    
    (send area restore-drawing-params)))

;; ===================================================================================================
;; Plot to various other backends

;; Plot to a bitmap
(defproc (plot3d-bitmap [renderer-tree (treeof (or/c renderer3d? non-renderer?))]
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

(defproc (plot3d-pict [renderer-tree (treeof (or/c renderer3d? non-renderer?))]
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
  (define saved-values (plot-parameters))
  (dc (λ (dc x y)
        (parameterize/group
         ([plot-parameters  saved-values])
         (plot3d/dc renderer-tree dc x y width height
                    #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:z-min z-min
                    #:z-max z-max #:angle angle #:altitude altitude #:title title #:x-label x-label
                    #:y-label y-label #:z-label z-label #:legend-anchor legend-anchor)))
      width height))

;; Plot to a snip
(defproc (plot3d-snip [renderer-tree (treeof (or/c renderer3d? non-renderer?))]
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
(defproc (plot3d-frame [renderer-tree (treeof (or/c renderer3d? non-renderer?))]
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
(defproc (plot3d-file [renderer-tree (treeof (or/c renderer3d? non-renderer?))]
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
(defproc (plot3d [renderer-tree (treeof (or/c renderer3d? non-renderer?))]
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
