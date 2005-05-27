(require (lib "animation.ss" "frtime")
         (lib "etc.ss" "frtime")
         (lib "gui.scm" "frtime"))

(define radius (make-slider "Radius" 100 200 150))
(define speed (* .02 (make-slider "Speed" 0 10 5)))
(define n (make-slider "Num circles" 1 10 6))
(define ratio (add1 (* .2 (make-slider "Ratio" 1 5 2))))

(define phase (wave speed))
(define center (make-posn 200 200))

(display-shapes
 (build-list
  n
  (lambda (i)
    (make-ring
     (posn+ center
            (posn*
             (make-posn (cos phase) (sin phase))
             (* radius (- 1 (expt ratio (- i))))))
     (/ radius (expt ratio i))
     "gray"))))
