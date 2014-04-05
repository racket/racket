#lang racket

;; These tests pass when they terminate

(require plot)

(printf "The following three plots should be blank:~n")
(plot (contour-intervals * -1 1 -1 1 #:alphas '()))
(plot3d (contour-intervals3d * -1 1 -1 1 #:alphas '()))
(plot3d (isosurfaces3d * -1 1 -1 1 -1 1 #:alphas '()))

(with-handlers ([exn?  (λ (_) (void))])
  (plot (stacked-histogram (list (vector 'a 1))
                           #:alphas '())))

(with-handlers ([exn?  (λ (_) (void))])
  (plot3d (stacked-histogram3d (list (vector 'a 'a 1))
                               #:alphas '())))
