(require (lib "plot.ss" "plot"))

(plot 
 (vector-field (gradient (lambda (x y) (* (sin x) (cos y)))) (samples 25))
 (title "gradient field of F(x,y) = sin(x) * sin(y)"))
