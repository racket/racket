#lang scribble/doc
@(require "common.rkt"
          (for-label pict))

@(define pen-eval (make-base-eval))
@(interaction-eval 
  #:eval pen-eval 
  (require racket/draw pict racket/class))

@defclass/title[pen% object% ()]{

A pen is a drawing tool with a color, width, and style. A pen draws
 lines and outlines, such as the outline of a rectangle. In a
 monochrome destination, all non-white pens are drawn as black.

In addition to its color, width, and style, a pen can have a @deftech{pen stipple}
 bitmap. Drawing with a stipple pen is similar to
 calling @method[dc<%> draw-bitmap] with the stipple bitmap in region
 painted by the pen.

A @deftech{pen style} is one of the following:

@itemize[

 @item{@indexed-racket['transparent] --- Draws with no effect (on the
       outline of the drawn shape).}

 @item{@indexed-racket['solid] --- Draws using the pen's color. If a
        (monochrome) @tech{pen stipple} is installed into the pen, black pixels
        from the stipple are transferred to the destination using the
        brush's color, and white pixels from the stipple are not
        transferred.}

 @item{@indexed-racket['xor] --- The same as @racket['solid], accepted 
        only for partial backward compatibility.}

 @item{@indexed-racket['hilite] --- Draws with black and a @racket[0.3] alpha.}

 @item{The following special pen modes use the pen's color, and they only
       apply when a @tech{pen stipple} is not used:
    @itemize[
  @item{@indexed-racket['dot]}
  @item{@indexed-racket['long-dash]}
  @item{@indexed-racket['short-dash]}
  @item{@indexed-racket['dot-dash]}
  @item{@indexed-racket['xor-dot]}
  @item{@indexed-racket['xor-long-dash]}
  @item{@indexed-racket['xor-short-dash]}
  @item{@indexed-racket['xor-dot-dash]}
  ]}

]

To avoid creating multiple pens with the same characteristics, use the
 global @racket[pen-list%] object @indexed-racket[the-pen-list], or
 provide a color, width, and style to @xmethod[dc<%> set-pen].

When drawing in @racket['smoothed] or @racket['aligned] mode, a pen's
 size is truncated after scaling to an integral size. A pen of size
 @racket[0] (after truncation, if applicable) uses a non-zero,
 scale-insensitive line size for the destination drawing context:
 @racket[1/4] unit (after scaling) for @racket[post-script-dc%] or
 @racket[pdf-dc%] contexts in @racket['smoothed] mode, or @racket[1]
 unit (after scaling) for any other context.  For example, in unscaled 
 canvas and bitmap contexts, a zero-width pen behaves the same as a
 pen of size @racket[1].

See also @racket[make-pen].


@defconstructor[([color (or/c string? (is-a?/c color%)) "black"]
                 [width (real-in 0 255) 0]
                 [style pen-style/c 'solid]
                 [cap pen-cap-style/c 'round]
                 [join pen-join-style/c 'round]
                 [stipple (or/c #f (is-a?/c bitmap%)) 
                          #f])]{

Creates a pen with the given color, width, @tech{pen style}, @tech{cap style}, @tech{join style}, and
 @tech{pen stipple} bitmap.  For the case that the color is specified using a name, see
 @racket[color-database<%>] for information about color names; if the
 name is not known, the pen's color is black.

}

@defmethod[(get-cap) pen-cap-style/c]{

Returns the pen @deftech{cap style}, which determines the shape of a line at
each of its ending points when drawn by @method[dc<%> draw-line] or at the
non-connecting ends of lines when drawn by @method[dc<%> draw-lines] or
@method[dc<%> draw-path]. The default is @racket['round], which draws the
end of a line as a semi-circle. The @racket['projecting] style draws a
square in place of the semi-circle (i.e., past the point at which the
line stops). The @racket['butt] style ends the line with a straight
edge, instead of projecting past the ending point of the line.

This code draws three diagonal lines, one with each of the possible caps
(@racket['round], @racket['butt], and then @racket['projecting]) and puts
a little red dot on the end points of the line.

@examples[#:eval 
          pen-eval
          (define (plot-line dc x1 y1 x2 y2 cap)
            (send dc set-pen 
                  (send the-pen-list find-or-create-pen
                        "black" 40 'solid cap))
            (send dc draw-line x1 y1 x2 y2)
            (send dc set-brush "red" 'solid)
            (send dc set-pen "black" 1 'transparent)
            (send dc draw-ellipse (- x1 4) (- y1 4) 8 8)
            (send dc draw-ellipse (- x2 4) (- y2 4) 8 8))
          
          (dc
           (λ (dc dx dy)
             (define old-pen (send dc get-pen))
             (define old-brush (send dc get-brush))
             
             (plot-line dc 20 30 80 90 'round)
             (plot-line dc 100 30 160 90 'butt)
             (plot-line dc 180 30 240 90 'projecting)
             
             (send dc set-pen old-pen)
             (send dc set-brush old-brush))
           270 120)]

}

@defmethod[(get-color)
           (is-a?/c color%)]{

Returns the pen's color object.

}

@defmethod[(get-join) pen-join-style/c]{

Returns the pen @deftech{join style} that is used between multiple lines
connected through @method[dc<%> draw-lines], @method[dc<%>
draw-rectangle], @method[dc<%> draw-polygon], or @method[dc<%>
draw-path].  The join style fills the space that would be left at the
outside corner of two lines if they were draw separately with
@racket['butt] line endings. The default join style is
@racket['round], which fills under an arc that lines up with the
outside of each of the two lines. The @racket['bevel] style fills in
the gap without adding extra pixels (i.e., it makes a blunt
corner). The @racket['miter] style fills the gap by adding pixels that
would be covered by both lines if they were extended past the corner
(i.e., it makes a sharp corner).

This code shows the three join styles
(@racket['round], @racket['bevel] and then @racket['miter])
by drawing a sequence
of lines, first with a sharp corner and then with a right-angle.
Each of the end points of the lines i with a red dot.

@examples[#:eval
          pen-eval
          (define points '((100 . 100)
                           (0 . 0)
                           (0 . 100)
                           (40 . 100)))
          
          (define (connect-points dc dx dy join)
            (send dc set-pen 
                  (send the-pen-list find-or-create-pen
                        "black" 40 'solid 'round join))
            (send dc draw-lines points dx dy)
            (send dc set-brush "red" 'solid)
            (send dc set-pen "black" 1 'transparent)
            (for ([pt (in-list points)])
              (send dc draw-ellipse 
                    (+ dx (car pt) -4) (+ dy (cdr pt) -4)
                    8 8)))
          
          (dc
           (λ (dc dx dy)
             (define old-pen (send dc get-pen))
             (define old-brush (send dc get-brush))
             
             (connect-points dc 20 50 'round)
             (connect-points dc 180 50 'bevel)
             (connect-points dc 340 50 'miter)
             
             (send dc set-pen old-pen)
             (send dc set-brush old-brush))
           460 170)]

}


@defmethod[(get-stipple)
           (or/c (is-a?/c bitmap%) #f)]{

Gets the current @tech{pen stipple} bitmap, or returns @racket[#f] if no stipple
 bitmap is installed.

}

@defmethod[(get-style) pen-style/c]{

Returns the @tech{pen style}. See @racket[pen%] for information about
possible styles.

}

@defmethod[(get-width)
           (real-in 0 255)]{

Returns the pen width.

}

@defmethod[(is-immutable?)
           boolean?]{

Returns @racket[#t] if the pen object is immutable.

}

@defmethod[(set-cap [cap-style pen-cap-style/c])
           void?]{

Sets the pen @tech{cap style}. See @method[pen% get-cap] for information about cap
 styles.

A pen cannot be modified if it was obtained from a @racket[pen-list%]
 or while it is selected into a drawing context.

}

@defmethod*[([(set-color [color (is-a?/c color%)])
              void?]
             [(set-color [color-name string?])
              void?]
             [(set-color [red byte?] [green byte?] [blue byte?])
              void?])]{

Sets the pen color.

A pen cannot be modified if it was obtained from a
 @racket[pen-list%] or while it is selected into a drawing context.

}

@defmethod[(set-join [join-style pen-join-style/c])
           void?]{

Sets the pen @tech{join style}. See @method[pen% get-join] for information about join
 styles.

A pen cannot be modified if it was obtained from a
 @racket[pen-list%] or while it is selected into a drawing context.

}

@defmethod[(set-stipple [bitmap (or/c (is-a?/c bitmap%) #f)])
           void?]{

Sets the pen @tech{pen stipple} bitmap, where @racket[#f] turns off the stipple bitmap.

If @racket[bitmap] is modified while is associated with a pen, the
 effect on the pen is unspecified. A pen cannot be modified if it was
 obtained from a @racket[pen-list%] or while it is selected into a
 drawing context.

}

@defmethod[(set-style [style pen-style/c])
           void?]{

Sets the @tech{pen style}. See @racket[pen%] for information about the
 possible styles.

A pen cannot be modified if it was obtained from a
 @racket[pen-list%] or while it is selected into a drawing context.

}

@defmethod[(set-width [width (real-in 0 255)])
           void?]{

Sets the pen width.

A pen cannot be modified if it was obtained from a
 @racket[pen-list%] or while it is selected into a drawing context.

}}

@close-eval[pen-eval]
