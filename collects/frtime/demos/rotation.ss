(require
 (lib "animation.ss" "frtime")
 (lib "gui.ss" "frtime"))

(define radius (make-slider "Radius" 50 150 100))
(define speed (* .01 (make-slider "Speed" -75 75 50)))
(define phase (wave speed))
(define x (cos phase))
(define y (sin phase))
(define green (+ .75 (* .25 x)))
(define blue (+ .75 (* .25 y)))

(define ring1?
  (make-check-box "Show x-y ring?"))

(define center?
  (make-check-box "Show center of x-y ring?"))

(define ring2?
  (make-check-box "Show pt-center ring?"))

(define (cvt z)
  (+ 200 (* radius z)))

(define shapes
  (list
   (when ring2? (make-ring (make-posn (cvt (/ x 4))
                                      (cvt (/ y 4)))
                           (* radius .75) "gray"))
   (when center? (make-circle (make-posn (cvt (- (/ x 2)))
                                         (cvt (- (/ y 2))))
                              (/ radius 12) "gray"))
   (when ring1? (make-ring (make-posn (cvt (- (/ x 2)))
                                      (cvt (- (/ y 2))))
                           (/ radius 2) "gray"))
   (make-ring (make-posn 200 200) radius "gray")
   (make-line (make-posn (cvt -1.2) 200) (make-posn (cvt 1.2) 200) "gray")
   (make-line (make-posn 200 (cvt -1.2)) (make-posn 200 (cvt 1.2)) "gray")
   (make-circle (make-posn (cvt x) (cvt y)) (/ radius 10)
                (make-rgb 0 green blue))
   (make-circle (make-posn 200 (cvt (- y))) (/ radius 10)
                (make-rgb 0 0.75 (- 1.5 blue)))
   (make-circle (make-posn (cvt (- x)) 200) (/ radius 10)
                (make-rgb 0 (- 1.5 green) 0.75))))

(display-shapes shapes)
