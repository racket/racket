#lang racket

(require plot plot/utils)

(plot-x-label #f)
(plot-y-label #f)

(define stops (list (* 2 (inexact->exact -max.0))
                    (inexact->exact -max.0)
                    (inexact->exact -min.0)
                    0
                    (inexact->exact +min.0)
                    (inexact->exact +max.0)
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
          (time (plot (lines (list (vector x-min y-min) (vector x-max y-max)))))))
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
          (time (plot3d (lines3d (list (vector x-min y-min z-min) (vector x-max y-max z-max)))))))
   (set! num-3d (+ 1 num-3d))))

(printf "Total plots: ~a~n" num-3d)
(printf "-----------------------------------------~n")
