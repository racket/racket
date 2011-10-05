#lang racket/base

(require racket/contract racket/match racket/class racket/snip racket/draw racket/string
         ;; Plotting
         "common/deprecation-warning.rkt"
         "common/contract.rkt"
         "common/contract-doc.rkt"
         "plot2d/line.rkt"
         "plot2d/contour.rkt"
         "plot2d/renderer.rkt"
         "plot3d/surface.rkt"
         "plot3d/renderer.rkt"
         "utils.rkt"
         "deprecated/renderers.rkt"
         ;; Curve fitting
         "deprecated/fit.rkt"
         ;; Miscellaneous
         "deprecated/math.rkt")

(provide mix line contour shade surface
         ;; Curve fitting
         (rename-out [fit-int fit])
         (struct-out fit-result)
         ;; Miscellaneous
         make-vec derivative gradient)

(define (mix . renderers)
  (deprecation-warning "mix" "list")
  (apply list renderers))

(defproc (line [f (real? . -> . (or/c real? (vector/c real? real?)))]
               [#:samples samples (integer>=/c 2) 150]
               [#:width width (real>=/c 0) 1]
               [#:color color plot-color/c 'red]
               [#:mode mode (one-of/c 'standard 'parametric) 'standard]
               [#:mapping mapping (one-of/c 'cartesian 'polar) 'cartesian]
               [#:t-min t-min real? -5] [#:t-max t-max real? 5]
               ) renderer2d?
  (deprecation-warning "line" "function, parametric or polar")
  (line-renderer f samples width color mode mapping t-min t-max))

(defproc (contour [f (real? real? . -> . real?)]
                  [#:samples samples (integer>=/c 2) 50]
                  [#:width width (real>=/c 0) 1]
                  [#:color color plot-color/c 'black]
                  [#:levels levels (or/c (integer>=/c 2) (listof real?)) 10]
                  ) renderer2d?
  (deprecation-warning "contour" "contours")
  (contour-renderer f samples width color levels))

(defproc (shade [f (real? real? . -> . real?)]
                [#:samples samples (integer>=/c 2) 50]
                [#:levels levels (or/c (integer>=/c 2) (listof real?)) 10]
                ) renderer2d?
  (deprecation-warning "shade" "contour-intervals")
  (shade-renderer f samples levels))

(defproc (surface [f (real? real? . -> . real?)]
                  [#:samples samples (integer>=/c 2) 50]
                  [#:width width (real>=/c 0) 1]
                  [#:color color plot-color/c 'black]
                  ) renderer3d?
  (deprecation-warning "surface" "surface3d")
  (surface-renderer f samples width color))
