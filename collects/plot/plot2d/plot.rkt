#lang racket/base

;; Procedures that plot 2D renderers.

(require racket/draw racket/snip racket/contract racket/list racket/class racket/match
         (for-syntax racket/base syntax/strip-context racket/syntax)
         "../common/math.rkt"
         "../common/contract.rkt" "../common/contract-doc.rkt"
         "../common/legend.rkt"
         "../common/file-type.rkt"
         "../common/area.rkt"
         "../common/parameters.rkt"
         "../common/deprecation-warning.rkt"
         "area.rkt"
         "renderer.rkt"
         "bounds.rkt")

(provide plot/dc plot plot-bitmap plot-snip plot-frame plot-file)

;; ===================================================================================================
;; Plot to a given device context

(defproc (plot/dc [renderer-tree (treeof renderer2d?)]
                  [dc (is-a?/c dc<%>)]
                  [#:x-min x-min (or/c real? #f) #f]
                  [#:x-max x-max (or/c real? #f) #f]
                  [#:y-min y-min (or/c real? #f) #f]
                  [#:y-max y-max (or/c real? #f) #f]
                  [#:title title (or/c string? #f) (plot-title)]
                  [#:x-label x-label (or/c string? #f) (plot-x-label)]
                  [#:y-label y-label (or/c string? #f) (plot-y-label)]
                  [#:legend-anchor legend-anchor anchor/c (plot-legend-anchor)]) void?
  (define rs (filter (λ (renderer) (not (renderer2d-out-of-bounds? renderer x-min x-max y-min y-max)))
                     (flatten (list renderer-tree))))
  
  (define-values (px-min px-max py-min py-max)
    (renderer2d-bounds-fixpoint rs x-min x-max y-min y-max))
  
  (let ([x-min  (if x-min x-min px-min)]
        [x-max  (if x-max x-max px-max)]
        [y-min  (if y-min y-min py-min)]
        [y-max  (if y-max y-max py-max)])
    (when (or (not x-min) (not x-max) (x-min . >= . x-max))
      (error 'plot "could not determine nonempty x axis; got: x-min = ~e, x-max = ~e" x-min x-max))
    (when (or (not y-min) (not y-max) (y-min . >= . y-max))
      (error 'plot "could not determine nonempty y axis; got: y-min = ~e, y-max = ~e" y-min y-max))
    
    (let ([x-min  (inexact->exact x-min)]
          [x-max  (inexact->exact x-max)]
          [y-min  (inexact->exact y-min)]
          [y-max  (inexact->exact y-max)])
      (define-values (all-x-ticks all-y-ticks)
        (for/lists (all-x-ticks all-y-ticks) ([r  (in-list rs)])
          ((renderer2d-ticks-fun r) x-min x-max y-min y-max)))
      
      (define x-ticks (remove-duplicates (append* all-x-ticks)))
      (define y-ticks (remove-duplicates (append* all-y-ticks)))
      
      (parameterize ([plot-title          title]
                     [plot-x-label        x-label]
                     [plot-y-label        y-label]
                     [plot-legend-anchor  legend-anchor])
        (define area (make-object 2d-plot-area%
                       x-ticks y-ticks x-min x-max y-min y-max dc))
        (send area start-plot)
        
        (define legend-entries
          (flatten (for/list ([renderer  (in-list rs)])
                     (match-define (renderer2d render-proc ticks-fun bounds-fun
                                               rx-min rx-max ry-min ry-max)
                       renderer)
                     (send area reset-drawing-params)
                     (send area clip-to-bounds rx-min rx-max ry-min ry-max)
                     (render-proc area))))
        
        (send area end-plot)
        
        (when (not (empty? legend-entries))
          (send area put-legend legend-entries))))))

;; ===================================================================================================
;; Plot to various other backends

;; Plot to a bitmap
(defproc (plot-bitmap [renderer-tree (treeof renderer2d?)]
                      [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
                      [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
                      [#:width width (integer>=/c 1) (plot-width)]
                      [#:height height (integer>=/c 1) (plot-height)]
                      [#:title title (or/c string? #f) (plot-title)]
                      [#:x-label x-label (or/c string? #f) (plot-x-label)]
                      [#:y-label y-label (or/c string? #f) (plot-y-label)]
                      [#:legend-anchor legend-anchor anchor/c (plot-legend-anchor)]
                      ) (is-a?/c bitmap%)
  (define bm (make-bitmap width height))
  (define dc (make-object bitmap-dc% bm))
  (plot/dc renderer-tree dc
           #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
           #:title title #:x-label x-label #:y-label y-label #:legend-anchor legend-anchor)
  bm)

;; Plot to a snip
(defproc (plot-snip [renderer-tree (treeof renderer2d?)]
                    [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
                    [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
                    [#:width width (integer>=/c 1) (plot-width)]
                    [#:height height (integer>=/c 1) (plot-height)]
                    [#:title title (or/c string? #f) (plot-title)]
                    [#:x-label x-label (or/c string? #f) (plot-x-label)]
                    [#:y-label y-label (or/c string? #f) (plot-y-label)]
                    [#:legend-anchor legend-anchor anchor/c (plot-legend-anchor)]
                    ) (is-a?/c snip%)
  (define bm
    (plot-bitmap
     renderer-tree
     #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:width width #:height height
     #:title title #:x-label x-label #:y-label y-label #:legend-anchor legend-anchor))
  (make-object image-snip% bm))

;; Plot to a frame
(defproc (plot-frame [renderer-tree (treeof renderer2d?)]
                     [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
                     [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
                     [#:width width (integer>=/c 1) (plot-width)]
                     [#:height height (integer>=/c 1) (plot-height)]
                     [#:title title (or/c string? #f) (plot-title)]
                     [#:x-label x-label (or/c string? #f) (plot-x-label)]
                     [#:y-label y-label (or/c string? #f) (plot-y-label)]
                     [#:legend-anchor legend-anchor anchor/c (plot-legend-anchor)]
                     ) (is-a?/c object%)
  (define make-snip-frame (dynamic-require 'plot/common/gui 'make-snip-frame))
  (define snip
    (plot-snip
     renderer-tree
     #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:width width #:height height
     #:title title #:x-label x-label #:y-label y-label #:legend-anchor legend-anchor))
  (make-snip-frame snip width height (if title (format "Plot: ~a" title) "Plot")))

;; Plot to a file
(defproc (plot-file [renderer-tree (treeof renderer2d?)]
                    [output (or/c path-string? output-port?)]
                    [kind (one-of/c 'auto 'png 'jpeg 'xmb 'xpm 'bmp 'ps 'pdf 'svg) 'auto]
                    [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
                    [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
                    [#:width width (integer>=/c 1) (plot-width)]
                    [#:height height (integer>=/c 1) (plot-height)]
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
                     [interactive (plot-ps-interactive?)] [parent #f] [use-paper-bbox #f] [as-eps #t]
                     [width width] [height height] [output output])]
         [(pdf)  (new pdf-dc%
                      [interactive (plot-pdf-interactive?)] [parent #f] [use-paper-bbox #f]
                      [width width] [height height] [output output])]
         [(svg)  (new svg-dc%
                      [width width] [height height] [output output] [exists 'truncate/replace])]))
     (send dc start-doc "Rendering plot")
     (send dc start-page)
     (plot/dc renderer-tree dc #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
              #:title title #:x-label x-label #:y-label y-label #:legend-anchor legend-anchor)
     (send dc end-page)
     (send dc end-doc)])
  (void))

;; Plot to a frame or a snip, depending on (plot-new-window?)
(defproc (plot [renderer-tree (treeof renderer2d?)]
               [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
               [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
               [#:width width (integer>=/c 1) (plot-width)]
               [#:height height (integer>=/c 1) (plot-height)]
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
