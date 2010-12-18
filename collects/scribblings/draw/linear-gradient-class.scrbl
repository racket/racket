#lang scribble/doc
@(require "common.ss"
          scribble/eval)

@(define class-eval (make-base-eval))
@(interaction-eval #:eval class-eval (require racket/class racket/draw))

@defclass/title[linear-gradient% object% ()]{

A linear gradient is a pattern tool that is used for
 filling in areas, such as the interior of a rectangle or ellipse.

A linear gradient has a control vector and color stops along the line.

@defconstructor[([x0 real? 0]
                 [y0 real? 0]
                 [x1 real? 0]
                 [y1 real? 0]
                 [stops (listof/c (list/c (real-in 0 1) (is-a?/c color%))) null])]{

Creates a linear gradient with the given line and color stops.
The gradients control vector is defined by its start point (x0, y0) and end point (x1, y1).
A color stop is a list containing an offset value between 0.0 and 1.0 and a @racket[color%].

}

@defmethod[(get-line)
           (values real? real? real? real?)]{

Returns the gradient's control line.

}

@defmethod[(get-stops)
           (listof/c (list/c (real-in/c 0 1) (is-a?/c color%)))]{

Returns the gradient's list of color stops.

}

}
@examples[ #:eval class-eval
  (define grad (new linear-gradient% 
    [x0 0] [y0 100] [x1 300] [y1 100] 
    [stops
      (list (list 0   (make-object color% 0 0 255))
            (list 0.5 (make-object color% 0 255 0))
            (list 1   (make-object color% 255 0 0)))]))

  (define grad2 (make-object linear-gradient% 0 0 100 300
    (list (list 0   (make-object color% 0 0 255))
          (list 0.5 (make-object color% 0 255 0))
          (list 1   (make-object color% 255 0 0)))))]


@(close-eval class-eval)
