#lang racket/base

(require racket/contract
         unstable/latent-contract/defthing
         unstable/contract
         pict
         "../common/contract.rkt"
         "../common/parameters.rkt"
         "../common/plot-element.rkt"
         "plot2d.rkt"
         "plot3d.rkt")

(provide (all-defined-out))

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
               ) pict?
  (define (call f . args)
    (apply f renderer-tree args
           #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:width width #:height height
           #:title title #:x-label x-label #:y-label y-label #:legend-anchor legend-anchor))
  
  (when out-file
    (call plot-file out-file out-kind))
  
  (call plot-pict))

(defproc (plot3d [renderer-tree (treeof (or/c renderer3d? nonrenderer?))]
                 [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
                 [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
                 [#:z-min z-min (or/c rational? #f) #f] [#:z-max z-max (or/c rational? #f) #f]
                 [#:width width exact-positive-integer? (plot-width)]
                 [#:height height exact-positive-integer? (plot-height)]
                 [#:angle angle real? #f] [#:altitude altitude real? #f]
                 [#:title title (or/c string? #f) (plot-title)]
                 [#:x-label x-label (or/c string? #f) (plot-x-label)]
                 [#:y-label y-label (or/c string? #f) (plot-y-label)]
                 [#:z-label z-label (or/c string? #f) (plot-z-label)]
                 [#:legend-anchor legend-anchor anchor/c (plot-legend-anchor)]
                 [#:out-file out-file (or/c path-string? output-port? #f) #f]
                 [#:out-kind out-kind (one-of/c 'auto 'png 'jpeg 'xmb 'xpm 'bmp 'ps 'pdf 'svg) 'auto]
                 ) pict?
  (define (call f . args)
    (apply f renderer-tree args
           #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:z-min z-min #:z-max z-max
           #:width width #:height height #:title title
           #:angle (or angle (plot3d-angle)) #:altitude (or altitude (plot3d-altitude))
           #:x-label x-label #:y-label y-label #:z-label z-label #:legend-anchor legend-anchor))
  
  (when out-file
    (call plot3d-file out-file out-kind))
  
  (call plot3d-pict))
