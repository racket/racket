#lang frtime
;; Needles
;; Written by Robb Cutler
;; July, 2004
;;
;; Demonstrates a grid of animated needles that follow the mouse cursor
;; using the FrTime language.


;; Require the animation library and the library
;; containing the build-list function.
(require frtime/animation
         frtime/gui)

;; How many needles on one side of the grid of needles
(define GRID-SIZE (make-slider "Grid size:" 1 12 8))

;; The length of a needle in pixels
(define NEEDLE-LENGTH 10)

;; The distance between the centers of two adjacent needles
(define SPACING 25)

;; A useful constant.
(define PI 3.1415626535)

(define (distance x1 y1 x2 y2)
  (sqrt (+ (sqr (- x1 x2)) (sqr (- y1 y2)))))

;; create-needle : number -> line
;; Define one needle.  A needle's orientation is such that it
;; lies on a line between the center of the needle and the 
;; position of mouse cursor. num is the needle's row-major
;; location on the grid (from 0 to GRID-SIZE^2-1).
(define (create-needle num)
  (let* ([row (quotient num GRID-SIZE)]
         [col (remainder num GRID-SIZE)]
         [x (+ (* row SPACING) SPACING)]
         [y (+ (* col SPACING) SPACING)]
         [delta-x (- (posn-x mouse-pos) x)]
         [delta-y (- (posn-y mouse-pos) y)]
         [theta1 (atan (/ delta-y (exact->inexact delta-x)))]
         [theta2 (- theta1 PI)]
         [x1-pos (+ x (* NEEDLE-LENGTH (cos theta1)))]
         [y1-pos (+ y (* NEEDLE-LENGTH (sin theta1)))]
         [x2-pos (+ x (* NEEDLE-LENGTH (cos theta2)))]
         [y2-pos (+ y (* NEEDLE-LENGTH (sin theta2)))]
         [color-density (min 1 (/ (distance x1-pos y1-pos (posn-x mouse-pos) (posn-y mouse-pos)) 150.0))]
         )
    (make-line (make-posn x1-pos y1-pos)
               (make-posn x2-pos y2-pos)
               (make-rgb (- 1 color-density) 0 0))))

;; Define a list of needles
(define needles (build-list (sqr GRID-SIZE) create-needle))

;; Draw the needles!
(display-shapes needles)
