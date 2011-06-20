#lang scribble/doc
@(require "common.rkt" scribble/eval
          (for-label slideshow/pict))

@(define class-eval (make-base-eval))
@(interaction-eval #:eval class-eval (require racket/class racket/draw slideshow/pict))

@defclass/title[linear-gradient% object% ()]{

A @deftech{linear gradient} is used with a @racket[brush%] to fill
 areas, such as the interior of a rectangle or ellipse, with smooth
 color transitions.

Colors transitions are based on a line, where colors are assigned to
 stop points along the line, and colors for in-between points are
 interpolated from the stop-point colors. The color of a point on the
 gradient's line is propagated to all points in the drawing context
 that are touched by a line through the point and perpendicular to the
 gradient's line.


@defconstructor[([x0 real?]
                 [y0 real?]
                 [x1 real?]
                 [y1 real?]
                 [stops (listof (list/c (real-in 0 1) (is-a?/c color%)))])]{

Creates a linear gradient with a line from (@racket[x0], @racket[y0])
to end point (@racket[x1], @racket[y1]). The @racket[stops] list
assigns colors to stop points along the line, where @racket[0.0]
corresponds to (@racket[x0], @racket[y0]), @racket[1.0] corresponds to
(@racket[x1], @racket[y2]), and numbers in between correspond to
points in between.

@examples[
  #:eval class-eval
  (define no-pen (make-object pen% "BLACK" 1 'transparent))
  (define brush-grad (new brush% [gradient 
    (new linear-gradient% 
         [x0 300] [y0 250] [x1 500] [y1 50]
         [stops
          (list (list 0   (make-object color% 255 0 0))
                (list 0.5 (make-object color% 0 255 0))
                (list 1   (make-object color% 0 0 255)))])]))

  (define brush-grad2 (new brush% [gradient
    (make-object linear-gradient% 
                 50 150 250 150
                 (list (list 0   (make-object color% 255 0 0))
                       (list 0.5 (make-object color% 0 255 0))
                       (list 1   (make-object color% 0 0 255))))]))
  (dc (lambda (dc x y)
    (send dc set-pen no-pen)
    (send dc set-brush brush-grad2)
    (send dc draw-ellipse 50 50 200 200)
        
    (send dc set-brush brush-grad)
    (send dc draw-rectangle 300 50 200 200)) 550 300)

]}


@defmethod[(get-line)
           (values real? real? real? real?)]{

Returns the gradient's control line as @racket[_x0], @racket[_y0],
@racket[_x1], and @racket[_y1].

}

@defmethod[(get-stops)
           (listof (list/c (real-in/c 0 1) (is-a?/c color%)))]{

Returns the gradient's list of color stops.

}}

@(close-eval class-eval)
