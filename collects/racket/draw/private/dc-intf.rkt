#lang scheme/base
(require scheme/class)

(provide dc<%>)

(define dc<%>
  (interface ()
    cache-font-metrics-key
    clear
    copy
    draw-arc
    draw-bitmap
    draw-bitmap-section
    draw-ellipse
    draw-line
    draw-lines
    draw-path
    draw-point
    draw-polygon
    draw-rectangle
    draw-rounded-rectangle
    draw-spline
    draw-text
    end-doc
    end-page
    erase
    flush
    get-alpha
    get-background
    get-brush
    get-char-height
    get-char-width
    get-clipping-region
    get-device-scale
    get-font
    get-gl-context
    get-initial-matrix
    get-origin
    get-pen
    get-rotation
    get-scale
    get-size
    get-smoothing
    get-text-background
    get-text-extent
    get-text-foreground
    get-text-mode
    get-transformation
    glyph-exists?
    ok?
    resume-flush
    rotate
    scale
    set-alpha
    set-background
    set-brush
    set-clipping-rect
    set-clipping-region
    set-font
    set-initial-matrix
    set-origin
    set-pen
    set-rotation
    set-scale
    set-smoothing
    set-text-background
    set-text-foreground
    set-text-mode
    set-transformation
    start-doc
    start-page
    suspend-flush
    transform
    translate
    try-color))
