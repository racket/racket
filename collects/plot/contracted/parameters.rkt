#lang racket/base

(require unstable/latent-contract)

(require "../common/parameters.rkt")
(provide
 (activate-contract-out
  plot-deprecation-warnings?
  ;; General plot parameters
  plot-x-axis? plot-y-axis? plot-z-axis?
  plot-x-far-axis? plot-y-far-axis? plot-z-far-axis?
  plot-x-tick-label-anchor
  plot-y-tick-label-anchor
  plot-x-far-tick-label-anchor
  plot-y-far-tick-label-anchor
  plot-x-tick-label-angle
  plot-y-tick-label-angle
  plot-x-far-tick-label-angle
  plot-y-far-tick-label-angle
  plot-width plot-height
  plot-foreground plot-foreground-alpha
  plot-background plot-background-alpha
  plot-line-width plot-tick-size
  plot-font-size plot-font-family
  plot-legend-anchor plot-legend-box-alpha
  plot-decorations?
  plot-animating?
  plot3d-samples
  plot3d-angle plot3d-altitude
  plot3d-ambient-light plot3d-diffuse-light? plot3d-specular-light?
  plot-new-window? plot-jpeg-quality plot-ps/pdf-interactive?
  plot-title
  plot-x-label plot-y-label plot-z-label
  plot-x-far-label plot-y-far-label plot-z-far-label
  plot-x-transform plot-x-ticks plot-x-far-ticks
  plot-y-transform plot-y-ticks plot-y-far-ticks
  plot-z-transform plot-z-ticks plot-z-far-ticks
  plot-d-ticks plot-r-ticks
  ;; Renderer parameters
  line-samples line-color line-width line-style line-alpha
  interval-color interval-style
  interval-line1-color interval-line1-width interval-line1-style
  interval-line2-color interval-line2-width interval-line2-style
  interval-alpha
  point-sym point-color point-size point-line-width point-alpha
  vector-field-samples
  vector-field-color vector-field-line-width vector-field-line-style
  vector-field-scale
  vector-field-alpha
  vector-field3d-samples
  error-bar-width error-bar-color error-bar-line-width error-bar-line-style error-bar-alpha
  contour-samples contour-levels contour-colors contour-widths contour-styles contour-alphas
  contour-interval-colors contour-interval-styles contour-interval-alphas
  rectangle-color rectangle-style
  rectangle-line-color rectangle-line-width rectangle-line-style
  rectangle-alpha
  rectangle3d-line-width
  discrete-histogram-gap discrete-histogram-skip discrete-histogram-invert?
  stacked-histogram-colors stacked-histogram-styles
  stacked-histogram-line-colors stacked-histogram-line-widths stacked-histogram-line-styles
  stacked-histogram-alphas
  x-axis-ticks? y-axis-ticks? z-axis-ticks?
  x-axis-labels? y-axis-labels? z-axis-labels?
  x-axis-far? y-axis-far? z-axis-far?
  x-axis-alpha y-axis-alpha z-axis-alpha
  polar-axes-number polar-axes-ticks? polar-axes-labels? polar-axes-alpha
  label-anchor label-angle label-alpha label-point-size
  surface-color surface-style surface-line-color surface-line-width surface-line-style surface-alpha
  contour-interval-line-colors contour-interval-line-widths contour-interval-line-styles
  isosurface-levels
  isosurface-colors isosurface-styles
  isosurface-line-colors isosurface-line-widths isosurface-line-styles
  isosurface-alphas
  ;; Functions
  pen-gap
  animated-samples
  default-contour-colors default-contour-fill-colors
  default-isosurface-colors default-isosurface-line-colors)
 ;; Parameter groups
 plot-parameters
 plot-axes?
 plot-tick-labels
 plot-appearance
 plot3d-appearance
 plot-output
 plot-labels
 plot-axes plot-x-axis plot-y-axis plot-z-axis)
