#lang scribble/doc
@(require "common.rkt" scribble/eval (for-label slideshow/pict))

@(define class-eval (make-base-eval))
@(interaction-eval #:eval class-eval (require racket/class racket/draw slideshow/pict))

@defclass/title[radial-gradient% object% ()]{

A @deftech{radial gradient} is used with a @racket[brush%] to fill
 areas, such as the interior of a rectangle or ellipse, with smooth
 color transitions.

Colors transitions are based on two circles and the sequence of circles that
 ``morph'' from the starting circle to the ending circle. Colors are
 assigned to stop circles in the sequence, and the colors of
 the start and end circles radiate inward and outward to points that
 are not on any intermediate circles.

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

@examples[
  #:eval class-eval
  (define no-pen (make-object pen% "BLACK" 1 'transparent))
  (define brush-grad (new brush% [gradient (new radial-gradient% 
    [x0 400] [y0 150] [r0 10] [x1 400] [y1 150] [r1 100]
    [stops
      (list (list 0   (make-object color% 255 0 0))
            (list 0.5 (make-object color% 0 255 0))
            (list 1   (make-object color% 0 0 255)))])]))

  (define brush-grad2 (new brush% [gradient
    (make-object radial-gradient% 150 150 0 150 150 100
                 (list (list 0   (make-object color% 0 0 255))
                       (list 0.5 (make-object color% 0 255 0))
                       (list 1   (make-object color% 255 0 0))))]))

  (dc (lambda (dc x y)
    (send dc set-pen no-pen)
    (send dc set-brush brush-grad2)
    (send dc draw-ellipse 50 50 200 200)
        
    (send dc set-brush brush-grad)
    (send dc draw-rectangle 300 50 200 200)) 550 300)
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
