#lang typed/racket

(require plot/typed/bitmap)

(define sin.png (make-temporary-file "plot-bitmap-sin-test~a.png"))
(plot (function sin -4 4)
      #:out-file sin.png)
(delete-file sin.png)

(define times.png (make-temporary-file "plot-bitmap-times-test~a.png"))
(plot3d (contour-intervals3d * -4 4 -4 4)
        #:out-file times.png)
(delete-file times.png)
