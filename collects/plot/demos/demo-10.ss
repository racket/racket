(require (lib "plot.ss" "plot"))

(require (lib "class.ss")
         (lib "plot-extend.ss" "plot"))
; (number -> number) mumbo-jumbo -> 2d-renderer
(define-plot-type dashed-line
  fun 2dview (x-min x-max) ((samples 100) (segments 20) (color 'red) (width 1))    
    (let* ((dash-size (/ (- x-max x-min) segments))
           (x-lists (build-list (/ segments 2) 
                                (lambda (index)                                    
                                  (x-values 
                                   (/ samples segments) 
                                   (+ x-min (* 2 index dash-size))
                                   (+ x-min (* (add1 ( * 2 index)) dash-size)))))))
      (send* 2dview 
        (set-line-color color)
        (set-line-width width))
      (for-each (lambda (dash)
                  (send 2dview plot-line 
                        (map (lambda (x) (vector x (fun x))) dash))) 
                x-lists)))
                     ;                                  

(plot (dashed-line (lambda (x) x) (color 'red)))
(plot3d (surface (lambda (x y) (* (sin x) (sin y)))))