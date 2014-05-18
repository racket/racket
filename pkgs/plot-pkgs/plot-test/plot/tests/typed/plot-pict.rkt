#lang typed/racket

(require typed/rackunit
         plot/typed/pict)

(require/typed
 pict
 [#:opaque Pict pict?])

(define sin.png (make-temporary-file "plot-bitmap-sin-test~a.png"))
(check-true (pict? (plot (function sin -4 4)
                         #:out-file sin.png)))
(delete-file sin.png)

(define times.png (make-temporary-file "plot-bitmap-times-test~a.png"))
(check-true (pict? (plot3d (contour-intervals3d * -4 4 -4 4)
                           #:out-file times.png)))
(delete-file times.png)
