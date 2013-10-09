#lang racket

(require rackunit
         plot/bitmap
         racket/draw)

(check-true (is-a? (plot (function sin -4 4)
                         #:out-file "sin.png")
                   bitmap%))
(check-true (is-a? (read-bitmap "sin.png") bitmap%))
(delete-file "sin.png")

(check-true (is-a? (plot3d (contour-intervals3d * -4 4 -4 4)
                           #:out-file "times.png")
                   bitmap%))
(check-true (is-a? (read-bitmap "times.png") bitmap%))
(delete-file "times.png")
