#lang racket/base

;; Procedures that plot 2D renderers.

(require racket/draw racket/snip racket/contract racket/list racket/class racket/match
         slideshow/pict
         unstable/parameter-group
         unstable/lazy-require
         "../common/contract.rkt"
         "../common/math.rkt"
         "../common/parameters.rkt"
         "../common/plot-element.rkt"
         "../common/file-type.rkt"
         "../common/deprecation-warning.rkt"
         "../common/contract-doc.rkt"
         "area.rkt")

;; Require lazily: without this, Racket complains while generating documentation:
;;   cannot instantiate `racket/gui/base' a second time in the same process
(lazy-require ["../common/gui.rkt" (make-snip-frame)])

(provide (all-defined-out))

;; ===================================================================================================
;; Plot to a given device context

(defproc (plot/dc [renderer-tree (treeof (or/c renderer2d? non-renderer?))]
                  [dc (is-a?/c dc<%>)]
                  [x real?] [y real?] [width (>=/c 0)] [height (>=/c 0)]
                  [#:x-min x-min (or/c real? #f) #f]
                  [#:x-max x-max (or/c real? #f) #f]
                  [#:y-min y-min (or/c real? #f) #f]
                  [#:y-max y-max (or/c real? #f) #f]
                  [#:title title (or/c string? #f) (plot-title)]
                  [#:x-label x-label (or/c string? #f) (plot-x-label)]
                  [#:y-label y-label (or/c string? #f) (plot-y-label)]
                  [#:legend-anchor legend-anchor anchor/c (plot-legend-anchor)]) void?
  (define given-bounds-rect (vector (ivl x-min x-max) (ivl y-min y-max)))
  (define rs (for/list ([r  (flatten (list renderer-tree))])
               (match r
                 [(non-renderer bounds-rect bounds-fun ticks-fun)
                  (renderer2d bounds-rect bounds-fun ticks-fun #f)]
                 [_  r])))
  
  (define plot-bounds-rect (bounds-fixpoint rs given-bounds-rect))
  
  (when (or (not (rect-regular? plot-bounds-rect))
            (rect-zero-area? plot-bounds-rect))
    (match-define (vector (ivl x-min x-max) (ivl y-min y-max)) plot-bounds-rect)
    (error 'plot "could not determine sensible plot bounds; determined x ∈ [~e,~e], y ∈ [~e,~e]"
           x-min x-max y-min y-max))
  
  (define bounds-rect (rect-inexact->exact plot-bounds-rect))
  
  (define-values (all-x-ticks all-x-far-ticks all-y-ticks all-y-far-ticks)
    (for/lists (all-x-ticks all-x-far-ticks all-y-ticks all-y-far-ticks) ([r  (in-list rs)])
      (define ticks-fun (plot-element-ticks-fun r))
      (cond [ticks-fun  (ticks-fun bounds-rect)]
            [else  (values empty empty empty empty)])))
  
  (define x-ticks (remove-duplicates (append* all-x-ticks)))
  (define y-ticks (remove-duplicates (append* all-y-ticks)))
  (define x-far-ticks (remove-duplicates (append* all-x-far-ticks)))
  (define y-far-ticks (remove-duplicates (append* all-y-far-ticks)))
  
  (parameterize ([plot-title          title]
                 [plot-x-label        x-label]
                 [plot-y-label        y-label]
                 [plot-legend-anchor  legend-anchor])
    (match-define (vector (ivl x-min x-max) (ivl y-min y-max)) bounds-rect)
    (define area (make-object 2d-plot-area%
                   x-ticks x-far-ticks y-ticks y-far-ticks
                   x-min x-max y-min y-max
                   dc x y width height))
    (send area start-plot)
    
    (define legend-entries
      (flatten (for/list ([rend  (in-list rs)])
                 (match-define (renderer2d rend-bounds-rect _bf _tf render-proc) rend)
                 (match-define (vector (ivl rx-min rx-max) (ivl ry-min ry-max))
                   (if rend-bounds-rect rend-bounds-rect (empty-rect 2)))
                 (send area start-renderer rx-min rx-max ry-min ry-max)
                 (if render-proc (render-proc area) empty))))
    
    (send area end-renderers)
    
    (when (not (empty? legend-entries))
      (send area draw-legend legend-entries))
    
    (send area end-plot)))

;; ===================================================================================================
;; Plot to various other backends

;; Plot to a bitmap
(defproc (plot-bitmap [renderer-tree (treeof (or/c renderer2d? non-renderer?))]
                      [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
                      [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
                      [#:width width exact-positive-integer? (plot-width)]
                      [#:height height exact-positive-integer? (plot-height)]
                      [#:title title (or/c string? #f) (plot-title)]
                      [#:x-label x-label (or/c string? #f) (plot-x-label)]
                      [#:y-label y-label (or/c string? #f) (plot-y-label)]
                      [#:legend-anchor legend-anchor anchor/c (plot-legend-anchor)]
                      ) (is-a?/c bitmap%)
  (define bm (make-bitmap width height))
  (define dc (make-object bitmap-dc% bm))
  (plot/dc renderer-tree dc 0 0 width height
           #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
           #:title title #:x-label x-label #:y-label y-label #:legend-anchor legend-anchor)
  bm)

(defproc (plot-pict [renderer-tree (treeof (or/c renderer2d? non-renderer?))]
                    [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
                    [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
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
(defproc (plot-snip [renderer-tree (treeof (or/c renderer2d? non-renderer?))]
                    [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
                    [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
                    [#:width width exact-positive-integer? (plot-width)]
                    [#:height height exact-positive-integer? (plot-height)]
                    [#:title title (or/c string? #f) (plot-title)]
                    [#:x-label x-label (or/c string? #f) (plot-x-label)]
                    [#:y-label y-label (or/c string? #f) (plot-y-label)]
                    [#:legend-anchor legend-anchor anchor/c (plot-legend-anchor)]
                    ) (is-a?/c image-snip%)
  (define bm
    (plot-bitmap
     renderer-tree
     #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:width width #:height height
     #:title title #:x-label x-label #:y-label y-label #:legend-anchor legend-anchor))
  (make-object image-snip% bm))

;; Plot to a frame
(defproc (plot-frame [renderer-tree (treeof (or/c renderer2d? non-renderer?))]
                     [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
                     [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
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
(defproc (plot-file [renderer-tree (treeof (or/c renderer2d? non-renderer?))]
                    [output (or/c path-string? output-port?)]
                    [kind (one-of/c 'auto 'png 'jpeg 'xmb 'xpm 'bmp 'ps 'pdf 'svg) 'auto]
                    [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
                    [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
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
(defproc (plot [renderer-tree (treeof (or/c renderer2d? non-renderer?))]
               [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
               [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
               [#:width width exact-positive-integer? (plot-width)]
               [#:height height exact-positive-integer? (plot-height)]
               [#:title title (or/c string? #f) (plot-title)]
               [#:x-label x-label (or/c string? #f) (plot-x-label)]
               [#:y-label y-label (or/c string? #f) (plot-y-label)]
               [#:legend-anchor legend-anchor anchor/c (plot-legend-anchor)]
               [#:out-file out-file (or/c path-string? output-port? #f) #f]
               [#:out-kind out-kind (one-of/c 'auto 'png 'jpeg 'xmb 'xpm 'bmp 'ps 'pdf 'svg) 'auto]
               [#:fgcolor fgcolor plot-color/c #f]
               [#:bgcolor bgcolor plot-color/c #f]
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
    
    (cond [(plot-new-window?)  (define frame (call plot-frame))
                               (send frame show #t)
                               (void)]
          [else  (call plot-snip)])))
