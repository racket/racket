#lang scribble/doc
@(require "common.rkt" scribble/eval (for-label slideshow/pict))

@(define class-eval (make-base-eval))
@(interaction-eval #:eval class-eval (require racket/class racket/draw slideshow/pict))

@defclass/title[radial-gradient% object% ()]{

A @deftech{radial gradient} is used with a @racket[brush%] to fill
 areas with smooth color transitions.
 Color transitions are based on two circles and the sequence of
 circles that ``morph'' from the starting circle to the ending
 circle. Normally, one of the two circles defining a gradient is
 nested within the other; in that case, points within the inner circle
 get the same color as the inner circle's edge, while points outside
 the outer circle get the same color as the outer circle's edge.

@defconstructor[([x0 real?]
                 [y0 real?]
                 [r0 real?]
                 [x1 real?]
                 [y1 real?]
                 [r1 real?]
                 [stops (listof (list/c (real-in 0 1) (is-a?/c color%)))])]{

Creates a radial gradient with the starting circle as the one with
radius @racket[r0] centered at (@racket[x0], @racket[y0]) and the
ending circle as the one with radius @racket[r1] centered at
(@racket[x1], @racket[y1]). The @racket[stops] list assigns colors to
circles, where @racket[0.0] corresponds to the starting circle,
@racket[1.0] corresponds to the ending circle, and numbers in between
correspond to circles in between.

The order of elements within @racket[stops] and duplicate points are
treated in the same way for as @racket[linear-gradient%].

@examples[
  #:eval class-eval
(define ellipse-brush 
  (new brush%
       [gradient
        (new radial-gradient% 
             [x0 100] [y0 100] [r0 0]
             [x1 100] [y1 100] [r1 100]
             [stops
              (list (list 0   (make-object color% 0 0 255))
                    (list 0.5 (make-object color% 0 255 0))
                    (list 1   (make-object color% 255 0 0)))])]))

(define rectangle-brush
  (new brush% 
       [gradient
        (new radial-gradient%
             [x0 100] [y0 100] [r0 10] 
             [x1 100] [y1 100] [r1 100]
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
   
   (send dc set-origin (+ ox dx 300) (+ oy dy 50))
   (send dc set-brush rectangle-brush)
   (send dc draw-rectangle 0 0 200 200)
   
   (send dc set-pen old-pen)
   (send dc set-brush old-brush)
   (send dc set-origin ox oy))
 550 300)  
]}

@defmethod[(get-circles)
           (values real? real? real? real? real? real?)]{

Returns the gradient's boundary circles as @racket[_x0], @racket[_y0],
@racket[_r0], @racket[_x1], @racket[_y1], and @racket[_r1].

}

@defmethod[(get-stops)
           (listof (list/c (real-in 0 1) (is-a?/c color%)))]{

Returns the gradient's list of color stops.

}}


@(close-eval class-eval)
