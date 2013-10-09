#lang racket

(require plot
         plot/private/plot2d/plot-area
         plot/private/plot3d/plot-area)

(parameterize ([plot-x-transform  log-transform]
               [plot-x-ticks      (log-ticks)])
  (values
   (plot (lines '(#(1 1) #(200 200))))
   (plot3d (lines3d '(#(1 1 1) #(200 200 200))))))

(plot2d-subdivisions)
(plot3d-subdivisions)

(time
 (parameterize ([plot3d-samples 4]
                [plot-x-transform  log-transform]
                [plot-x-ticks      (log-ticks)]
                [plot-y-transform  log-transform]
                [plot-y-ticks      (log-ticks)])
   (values
    (plot (lines '(#(1 1) #(200 200))))
    (plot3d (surface3d + 1 200 1 200)))))

(plot2d-subdivisions)
(plot3d-subdivisions)

(time
 (parameterize ([plot-x-transform  (collapse-transform -1 1)])
   (values
    (plot (lines '(#(-2 -2) #(2 2))))
    (plot3d (surface3d + -2 2 -2 2)))))

(plot2d-subdivisions)
(plot3d-subdivisions)
