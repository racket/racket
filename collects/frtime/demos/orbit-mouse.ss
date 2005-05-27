(require (lib "animation.ss" "frtime")
         (lib "gui.ss" "frtime"))

(define radius 25)
(define speed (* .1 (make-slider "Speed" -15 15 6)))
(define phase (wave speed))
(define n (make-slider "# Balls" 1 6 3))
(define r (/ (make-slider "Red%"   0 100 0) 100.0))
(define g (/ (make-slider "Green%" 0 100 0) 100.0))
(define b (/ (make-slider "Blue%"  0 100 0) 100.0))

(display-shapes
 (build-list
  n
  (lambda (i)
    (let ([t (+ (/ (* 2 pi i) n) phase)])
      (make-circle
       (posn+ mouse-pos
              (make-posn
               (* radius (cos t))
               (* radius (sin t))))
       5 (make-rgb r g b))))))
