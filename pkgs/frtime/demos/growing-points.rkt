#lang frtime
;; GrowingPoints
;; Written by Robb Cutler
;; July, 2004
;;
;; Demonstrates a grid of points that grow as the mouse cursor
;; approaches them. Uses the FrTime language.


;; Require the animation library, the gui library, and the library
;; containing the build-list function.
(require frtime/animation
         frtime/gui)

;; distance : number number number number -> number
;; Returns the distance between (x1, y1) and (x2, y2).
(define (distance x1 y1 x2 y2)
  (sqrt (+ (sqr (- x1 x2)) (sqr (- y1 y2)))))

;; How many growing points on one side of the grid of growing points.
(define GRID-SIZE (make-slider "Width" 1 12 8))

;; The distance between the centers of two adjacent growing points.
(define grid-resolution (make-slider "Resolution" 2 30 20))

;; The maximum size a growing point can grow.
(define max-point-size (make-slider "Max Point Size" 2 30 20))

;; create-growing-point : number -> line
;; Define one growing point.  A point's size is such that it
;; is larger as its distance to the position of the mouse
;; cursor decreases. num is the needle's row-major
;; location on the grid (from 0 to GRID-SIZE^2-1).
(define (create-growing-point num)
  (let* ([row (quotient num GRID-SIZE)]
         [col (remainder num GRID-SIZE)]
         [x (+ (* row grid-resolution) 25)]
         [y (+ (* col grid-resolution) 25)]
         [d (distance x y (posn-x mouse-pos) (posn-y mouse-pos))]
         [pt-size (if (< d (- max-point-size 2))
                      (- max-point-size d)
                      2)])
    (make-circle (make-posn x y)
                 pt-size
                 (make-rgb (/ pt-size max-point-size) 0 0))))

;; Define a list of growing points.
(define growing-points (build-list (sqr GRID-SIZE) create-growing-point))

;; Draw the growing points!
(display-shapes growing-points)
