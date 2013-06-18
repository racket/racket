#lang typed/racket

;; This test ensures that the types of plot parameters are
;; correct and can be set appropriately in TR.

(require plot/typed)

(interval-line1-style 1)
(interval-line2-style 3)
(plot (function-interval sub1 add1 0 1))

(rectangle-line-style 2)
(plot (rectangles (list (list (ivl 0 1) (ivl 0 1))
                        (list (ivl 1 2) (ivl 1 2)))))

;; ---------------------------------------------------------------------------------------------------
;; The following two plots should be the same

(contour-colors (λ (n) '((255 0 0) (0 0 255))))
(contour-widths (λ (n) '(.25 .5 .75)))
(contour-styles (λ (n) '(0 1 2)))
(contour-interval-colors (λ (n) '(1 2 3)))
(contour-interval-styles (λ (n) '(0 1 2)))
(contour-interval-alphas (λ (n) '(0.25 0.5 0.75)))
(plot (contour-intervals + 0 1 0 1))

(contour-colors '((255 0 0) (0 0 255)))
(contour-widths '(.25 .5 .75))
(contour-styles '(0 1 2))
(contour-interval-colors '(1 2 3))
(contour-interval-styles '(0 1 2))
(contour-interval-alphas '(0.25 0.5 0.75))
(plot (contour-intervals + 0 1 0 1))

;; ---------------------------------------------------------------------------------------------------
;; The following two plots should be the same

(stacked-histogram-alphas (λ (n) '(0.25 0.5 0.75)))
(stacked-histogram-colors (λ (n) '(1 2 3)))
(stacked-histogram-line-styles (λ (n) '(0 1 2)))
(stacked-histogram-styles (λ (n) '(0 1 2)))
(plot (stacked-histogram (list (list 'a '(1 1 1)) (list 'b '(1.5 3))
                               (list 'c '()) (list 'd '(1/2)))))

(stacked-histogram-alphas '(0.25 0.5 0.75))
(stacked-histogram-colors '(1 2 3))
(stacked-histogram-line-styles '(0 1 2))
(stacked-histogram-styles '(0 1 2))
(plot (stacked-histogram (list (list 'a '(1 1 1)) (list 'b '(1.5 3))
                               (list 'c '()) (list 'd '(1/2)))))

;; ---------------------------------------------------------------------------------------------------
;; The following two plots should be the same

(contour-interval-line-colors (λ (n) '(1 2 3)))
(contour-interval-line-widths (λ (n) '(0.25 0.5 0.75)))
(contour-interval-line-styles (λ (n) '(1 2 3)))
(plot3d (contour-intervals3d + 0 1 0 1))

(contour-interval-line-colors '(1 2 3))
(contour-interval-line-widths '(0.25 0.5 0.75))
(contour-interval-line-styles '(1 2 3))
(plot3d (contour-intervals3d + 0 1 0 1))

;; ---------------------------------------------------------------------------------------------------
;; The following two plots should be the same

(isosurface-colors (λ (n) '(1 2 3)))
(isosurface-styles (λ (n) '(0 1 2)))
(isosurface-line-colors (λ (n) '(3 2 1)))
(isosurface-line-widths (λ (n) '(0.25 0.5 0.75)))
(isosurface-line-styles (λ (n) '(0 1 2)))
(isosurface-alphas (λ (n) '(0.25 0.5 0.75)))
(plot3d (isosurfaces3d - 0 1 0 1 0 1))

(isosurface-colors '(1 2 3))
(isosurface-styles '(0 1 2))
(isosurface-line-colors '(3 2 1))
(isosurface-line-widths '(0.25 0.5 0.75))
(isosurface-line-styles '(0 1 2))
(isosurface-alphas '(0.25 0.5 0.75))
(plot3d (isosurfaces3d - 0 1 0 1 0 1))

