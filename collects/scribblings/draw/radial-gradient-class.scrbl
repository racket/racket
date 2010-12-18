#lang scribble/doc
@(require "common.ss"
          scribble/eval)

@(define class-eval (make-base-eval))
@(interaction-eval #:eval class-eval (require racket/class racket/draw))

@defclass/title[radial-gradient% object% ()]{

A radial gradient is a pattern tool that is used for
 filling in areas, such as the interior of a circle or ellipse.

A radial gradient has two circles boundaries and color stops between the circles.

@defconstructor[([x0 real? 0]
                 [y0 real? 0]
                 [r0 real? 0]
                 [x1 real? 0]
                 [y1 real? 0]
                 [r1 real? 0]
                 [stops (listof/c (list/c (real-in 0 1) (is-a?/c color%))) null])]{

Creates a radial gradient with the given circles boundaries and color stops.
Point (x0, y0) and radius r0 define the start bounding circle, while
point (x1, y1) and radius r1 define the end bounding circle.
The gradient's control vector extends from any point on the start circle to the
corresponding point on the end circle.
A color stop is a list containing an offset value between 0.0 and 1.0 and a @racket[color%].

}

@defmethod[(get-circles)
           (values real? real? real? real? real? real?)]{

Returns the gradient's boundary circles.

}

@defmethod[(get-stops)
           (listof/c (list/c (real-in 0 1) (is-a?/c color%)))])]{

Returns the gradient's list of color stops.

}

}
@examples[ #:eval class-eval
  (define grad (new radial-gradient% 
    [x0 0] [y0 100] [r0 10] [x1 300] [y1 100] [r1 100]
    [stops
      (list (list 0   (make-object color% 0 0 255))
            (list 0.5 (make-object color% 0 255 0))
            (list 1   (make-object color% 255 0 0)))]))

  (define grad2 (make-object radial-gradient% 150 150 0 150 150 100
    (list (list 0   (make-object color% 0 0 255))
          (list 0.5 (make-object color% 0 255 0))
          (list 1   (make-object color% 255 0 0)))))]

@(close-eval class-eval)


