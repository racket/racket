  (define-private-class dc% dc<%> object% () #f
    cache-font-metrics-key
    get-alpha
    set-alpha
    glyph-exists?
    end-page
    end-doc
    start-page
    start-doc
    ok?
    get-gl-context
    get-size
    get-text-foreground
    get-text-background
    get-pen
    get-font
    get-brush
    get-text-mode
    get-background
    get-origin
    get-scale
    set-origin
    set-scale
    set-text-mode
    try-color
    draw-bitmap
    draw-bitmap-section
    get-char-width
    get-char-height
    get-text-extent
    get-smoothing
    set-smoothing
    set-text-foreground
    set-text-background
    set-brush
    set-pen
    set-font
    set-background
    get-clipping-region
    set-clipping-region
    set-clipping-rect
    draw-polygon
    draw-lines
    draw-path
    draw-ellipse
    draw-arc
    draw-text
    draw-spline
    draw-rounded-rectangle
    draw-rectangle
    draw-point
    draw-line
    clear)
  (define-function draw-tab)
  (define-function draw-tab-base)
  (define-class bitmap-dc% dc% () ()
    get-bitmap
    set-bitmap
    draw-bitmap-section-smooth
    set-argb-pixels
    get-argb-pixels
    set-pixel
    get-pixel)
  (define-class post-script-dc% dc% () ([interactive #t] [parent #f] [use-paper-bbox #f] [eps #t]))
  (define-class printer-dc% dc% () ([parent #f]))
  (define-private-class gl-context% gl-context<%> object% () #f
    call-as-current
    swap-buffers
    ok?)
  (define-class gl-config% object% () #f
    get-double-buffered
    set-double-buffered
    get-stereo
    set-stereo
    get-stencil-size
    set-stencil-size
    get-accum-size
    set-accum-size
    get-depth-size
    set-depth-size
    get-multisample-size
    set-multisample-size)
