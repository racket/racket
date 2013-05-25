#lang scribble/doc
@(require "common.rkt" scribble/eval
          (for-label pict))

@(define class-eval (make-base-eval))
@(interaction-eval #:eval class-eval (require racket/class racket/draw pict))

@defclass/title[linear-gradient% object% ()]{

A @deftech{linear gradient} is used with a @racket[brush%] to fill
 areas with smooth color transitions.
 Color transitions are based on a line, where colors are assigned to
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

Elements in @racket[stops] are implicitly sorted by point (i.e., by
the number between @racket[0.0] and @racket[1.0]). Order is preserved
for multiple elements for the same point, in which case the first
element for a given point is treated infinitesimally before the point,
and additional elements between the first and last for a stop point
are effectively ignored.

@examples[
  #:eval class-eval
(define ellipse-brush 
  (new brush% 
       [gradient
        (new linear-gradient%
             [x0 0] 
             [y0 200]
             [x1 200]
             [y1 00]
             [stops 
              (list (list 0   (make-object color% 255 0 0))
                    (list 0.5 (make-object color% 0 255 0))
                    (list 1   (make-object color% 0 0 255)))])]))

(define rectangle-brush
  (new brush% 
       [gradient
        (new linear-gradient%
             [x0 0]
             [y0 100]
             [x1 100]
             [y1 0]
             [stops 
              (list (list 0   (make-object color% 255 0 0))
                    (list 0.5 (make-object color% 0 255 0))
                    (list 1   (make-object color% 0 0 255)))])]))

(dc
 (λ (dc dx dy)
   (define old-pen (send dc get-pen))
   (define old-brush (send dc get-brush))
   (define-values (ox oy) (send dc get-origin))
   (send dc set-pen "black" 1 'transparent)
   (send dc set-brush ellipse-brush)
   
   (send dc set-origin (+ ox dx 50) (+ oy dy 50))
   (send dc draw-ellipse 0 0 200 200)
   
   (send dc set-brush rectangle-brush)
   (send dc set-origin (+ ox dx 300) (+ oy dy 50))
   (send dc draw-rectangle 0 0 200 200)
   
   (send dc set-pen old-pen)
   (send dc set-brush old-brush)
   (send dc set-origin ox oy))
 550 300)
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
