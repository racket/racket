(require (lib "plot.ss" "plot"))

(define (trig x y) (* (sin x) (sin y)))


(plot (mix
       (shade trig)
       (contour trig)
       (vector-field (gradient trig ) (samples 25)))
      (x-min -1.5) (x-max 1.5) (y-min -1.5) (y-max 1.5) 
      (title "gradient field +shdade + contours of F(x,y) = sin(x) * sin(y)"))

(plot3d (mesh3d trig)
     (x-min -3.5) (x-max 3.5) (y-min -3.5) (y-max 3.5) (z-min -1.0) (z-max 1.5) (bgcolor '(0 0 0)) (fgcolor '(255 0 0)))   
