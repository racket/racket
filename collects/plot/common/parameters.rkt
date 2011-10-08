#lang racket/base

;; Parameters that control the look and behavior of plots.

(require racket/contract
         "contract.rkt" "contract-doc.rkt"
         "draw.rkt"
         "axis-transform.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Parameters common to 2D and 3D

(defparam plot-deprecation-warnings? boolean? #f)

;; Output

(defparam plot-width exact-positive-integer? 400)
(defparam plot-height exact-positive-integer? 400)
(defparam plot-new-window? boolean? #f)
(defparam plot-jpeg-quality (integer-in 0 100) 100)
(defparam plot-ps-interactive? boolean? #f)
(defparam plot-pdf-interactive? boolean? #f)

;; General appearance

(defparam plot-foreground color plot-color/c 0)
(defparam plot-background color plot-color/c 0)
(defparam plot-foreground-alpha alpha (real-in 0 1) 1)
(defparam plot-background-alpha alpha (real-in 0 1) 1)
(defparam plot-font-size size (>=/c 0) 11)
(defparam plot-font-family family font-family/c 'roman)
(defparam plot-line-width width (>=/c 0) 1)

(define (pen-gap) (* 2 (plot-line-width)))

(defparam plot-legend-anchor anchor anchor/c 'top-right)
(defparam plot-legend-box-alpha alpha (real-in 0 1) 2/3)

(defparam plot-tick-size (>=/c 0) 10)
(defparam plot-tick-skip exact-positive-integer? 2)

(defparam plot-title (or/c string? #f) #f)
(defparam plot-x-label (or/c string? #f) "x axis")
(defparam plot-y-label (or/c string? #f) "y axis")
(defparam plot-z-label (or/c string? #f) #f)

;; Lines

(defparam line-samples (and/c exact-integer? (>=/c 2)) 500)
(defparam line-color plot-color/c 1)
(defparam line-width (>=/c 0) 1)
(defparam line-style plot-pen-style/c 'solid)
(defparam line-alpha (real-in 0 1) 1)

;; Intervals

(defparam interval-color plot-color/c 3)
(defparam interval-style plot-brush-style/c 'solid)
(defparam interval-line1-color plot-color/c 3)
(defparam interval-line1-width (>=/c 0) 1)
(defparam interval-line1-style plot-pen-style/c 'solid)
(defparam interval-line2-color plot-color/c 3)
(defparam interval-line2-width (>=/c 0) 1)
(defparam interval-line2-style plot-pen-style/c 'solid)
(defparam interval-alpha (real-in 0 1) 3/4)

;; Points

(defparam point-sym point-sym/c 'circle)
(defparam point-color plot-color/c 0)
(defparam point-size (>=/c 0) 6)
(defparam point-line-width (>=/c 0) 1)
(defparam point-alpha (real-in 0 1) 1)

;; Vector fields

(defparam vector-field-samples exact-positive-integer? 20)
(defparam vector-field-color plot-color/c 1)
(defparam vector-field-line-width (>=/c 0) 2/3)
(defparam vector-field-line-style plot-pen-style/c 'solid)
(defparam vector-field-scale (or/c real? (one-of/c 'auto 'normalized)) 'auto)
(defparam vector-field-alpha (real-in 0 1) 1)

;; Error bars

(defparam error-bar-width (>=/c 0) 6)
(defparam error-bar-color plot-color/c 0)
(defparam error-bar-line-width (>=/c 0) 1)
(defparam error-bar-line-style plot-pen-style/c 'solid)
(defparam error-bar-alpha (real-in 0 1) 2/3)

;; Contours

(defproc (default-contour-colors [zs (listof real?)]) (listof plot-color/c)
  (color-seq* (list (->pen-color 5) (->pen-color 0) (->pen-color 1))
              (length zs)))

(defproc (default-contour-fill-colors [zs (listof real?)]) (listof plot-color/c)
  (color-seq* (list (->brush-color 5) (->brush-color 0) (->brush-color 1))
              (sub1 (length zs))))

(defparam contour-samples (and/c exact-integer? (>=/c 2)) 51)
(defparam contour-levels (or/c 'auto exact-positive-integer? (listof real?)) 'auto)
(defparam contour-colors plot-colors/c default-contour-colors)
(defparam contour-widths pen-widths/c '(1))
(defparam contour-styles plot-pen-styles/c '(solid long-dash))
(defparam contour-alphas alphas/c '(1))

(defparam contour-interval-colors plot-colors/c default-contour-fill-colors)
(defparam contour-interval-styles plot-brush-styles/c '(solid))
(defparam contour-interval-alphas alphas/c '(1))

;; Histograms

(defparam rectangle-color plot-color/c 3)
(defparam rectangle-style plot-brush-style/c 'solid)
(defparam rectangle-line-color plot-color/c 3)
(defparam rectangle-line-width (>=/c 0) 1)
(defparam rectangle-line-style plot-pen-style/c 'solid)
(defparam rectangle-alpha (real-in 0 1) 1)
(defparam discrete-histogram-gap (real-in 0 1) 1/8)

;; Decorations

(defparam x-axis-ticks? boolean? #t)
(defparam y-axis-ticks? boolean? #t)
(defparam z-axis-ticks? boolean? #t)

(defparam polar-axes-number exact-positive-integer? 12)
(defparam polar-axes-ticks? boolean? #t)

(defparam label-anchor anchor/c 'left)
(defparam label-angle real? 0)
(defparam label-alpha (real-in 0 1) 1)
(defparam label-point-size (>=/c 0) 4)

;; Sampling

(defparam plot-x-transform (real? real? . -> . invertible-function?) id-transform)
(defparam plot-y-transform (real? real? . -> . invertible-function?) id-transform)
(defparam plot-z-transform (real? real? . -> . invertible-function?) id-transform)

;; ===================================================================================================
;; 3D-specific parameters

;; General appearance

(defparam plot3d-samples (and/c exact-integer? (>=/c 2)) 41)
(defparam plot3d-animating? boolean? #f)
(defparam plot3d-angle real? 30)
(defparam plot3d-altitude real? 60)
(defparam plot3d-ambient-light-value (real-in 0 1) 2/3)
(defparam plot3d-diffuse-light? boolean? #t)
(defparam plot3d-specular-light? boolean? #t)

(defproc (samples/animating? [samples (and/c exact-integer? (>=/c 2))]
                             ) (and/c exact-integer? (>=/c 2))
  (cond [(plot3d-animating?)  (max 2 (ceiling (* 1/4 samples)))]
        [else  samples]))

;; Surfaces

(defparam surface-color plot-color/c 0)
(defparam surface-style plot-brush-style/c 'solid)
(defparam surface-line-color plot-color/c 0)
(defparam surface-line-width (>=/c 0) 1/3)
(defparam surface-line-style plot-pen-style/c 'solid)
(defparam surface-alpha (real-in 0 1) 1)

;; Contour surfaces

(defparam contour-interval-line-colors plot-colors/c '(0))
(defparam contour-interval-line-widths pen-widths/c '(1/3))
(defparam contour-interval-line-styles plot-pen-styles/c '(solid))

;; Isosurfaces

(defproc (default-isosurface-colors [zs (listof real?)]) (listof plot-color/c)
  (color-seq* (list (->brush-color 5) (->brush-color 0) (->brush-color 1))
              (length zs)))

(defproc (default-isosurface-line-colors [zs (listof real?)]) (listof plot-color/c)
  (color-seq* (list (->pen-color 5) (->pen-color 0) (->pen-color 1))
              (length zs)))

(defparam isosurface-levels exact-positive-integer? 3)
(defparam isosurface-colors plot-colors/c default-isosurface-colors)
(defparam isosurface-line-colors plot-colors/c default-isosurface-line-colors)
(defparam isosurface-line-widths pen-widths/c '(1/3))
(defparam isosurface-line-styles plot-pen-styles/c '(solid))
(defparam isosurface-alphas alphas/c '(1/2))

;; Histograms

(defparam rectangle3d-line-width (>=/c 0) 1/3)
