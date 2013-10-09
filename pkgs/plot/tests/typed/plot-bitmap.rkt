#lang typed/racket

(require plot/typed/bitmap)

(plot (function sin -4 4)
      #:out-file "sin.png")
(delete-file "sin.png")

(plot3d (contour-intervals3d * -4 4 -4 4)
        #:out-file "times.png")
(delete-file "times.png")
