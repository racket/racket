#lang racket

(require rackunit
         plot/pict
         pict
         racket/draw)

(define sin.png (make-temporary-file "plot-bitmap-sin-test~a.png"))
(check-true (pict? (plot (function sin -4 4)
                         #:out-file sin.png)))
(check-true (is-a? (read-bitmap sin.png) bitmap%))
(delete-file sin.png)

(define times.png (make-temporary-file "plot-bitmap-times-test~a.png"))
(check-true (pict? (plot3d (contour-intervals3d * -4 4 -4 4)
                           #:out-file times.png)))
(check-true (is-a? (read-bitmap times.png) bitmap%))
(delete-file times.png)
