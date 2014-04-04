#lang racket

(require plot plot/utils unstable/flonum)

(module+ test
  (module config info
    (define timeout 150)))

(printf "Point at 0,0:~n")
(plot (points '(#(0 0)))
      #:x-min +min.0 #:x-max (flstep +min.0 1000)
      #:y-min 0 #:y-max 1)

(printf "Point at 0,0,0:~n")
(plot3d (points3d '(#(0 0 0)))
      #:x-min +min.0 #:x-max (flstep +min.0 1000)
      #:y-min 0 #:y-max 1 #:z-min 0 #:z-max 1)

(printf "Steps should appear all the way from the bottom to the top:~n")
(plot (function exact->inexact (flstep 0.0 -1) (flstep 0.0 2)))

(printf "Steps should appear all the way from the bottom to the top:~n")
(plot3d (surface3d (λ (x y) (exact->inexact x))
                   (flstep 0.0 -1) (flstep 0.0 2)
                   (flstep 0.0 -1) (flstep 0.0 2)))

(plot-x-label #f)
(plot-y-label #f)

(define stops (list (* 2 (inexact->exact -max.0))
                    -max.0 -min.0 0.0 +min.0 +max.0
                    (* 2 (inexact->exact +max.0))))

(define (extreme-real->string x)
  (real->plot-label x (digits-for-range 0 (abs x))))

(define num-2d 0)
(time
 (for ([x-min  (in-list stops)]
       [x-max  (in-list (rest stops))]
       #:when #t
       [y-min  (in-list stops)]
       [y-max  (in-list (rest stops))])
   (displayln
    (list (format "[~a,~a] × [~a,~a]"
                  (extreme-real->string x-min) (extreme-real->string x-max)
                  (extreme-real->string y-min) (extreme-real->string y-max))
          (time (plot (lines (list (vector x-min y-min)
                                   (vector x-max y-max)))))))
   (set! num-2d (+ 1 num-2d))))

(printf "Total 2D plots: ~a~n" num-2d)
(printf "-----------------------------------------~n")

(define num-3d 0)

(time
 (for ([x-min  (in-list stops)]
       [x-max  (in-list (rest stops))]
       #:when #t
       [y-min  (in-list stops)]
       [y-max  (in-list (rest stops))]
       #:when #t
       [z-min  (in-list stops)]
       [z-max  (in-list (rest stops))])
   (displayln
    (list (format "[~a,~a] × [~a,~a] × [~a,~a]"
                  (extreme-real->string x-min) (extreme-real->string x-max)
                  (extreme-real->string y-min) (extreme-real->string y-max)
                  (extreme-real->string z-min) (extreme-real->string z-max))
          (time (plot3d (lines3d (list (vector x-min y-min z-min)
                                       (vector x-max y-max z-max)))))))
   (set! num-3d (+ 1 num-3d))))

(printf "Total plots: ~a~n" num-3d)
(printf "-----------------------------------------~n")
