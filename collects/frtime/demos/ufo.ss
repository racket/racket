(require (lib "animation.ss" "frtime"))

(define ufo-x
  (+ 200 ; center of window
     (integral ; integrate over time
      (* .04 ; scale speed to appropriate # of pixels/ms
         (- 3 ; start off stationary
            ; use left and right arrows to accelerate
            ; (up to 3 in either direction)
            (range-control (key 'left) (key 'right) 6 3))))))
(define ufo-y ; bob up and down 5 pixels around center of window
  (+ 200 (* 5 (sin (/ milliseconds 200)))))
(define ufo-bright ; flash light and dark
  (/ (add1 (cos (/ milliseconds 100))) 3))
(define ufo-color
  (make-rgb ufo-bright ufo-bright ufo-bright))

(display-shapes
 (list
  (make-circle (make-posn ufo-x ufo-y) 10 ufo-color)
  (make-rect (make-posn (- ufo-x 20) (- ufo-y 2))
             40 4 ufo-color)))
