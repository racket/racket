#lang scribble/doc
@(require "common.rkt")

@defclass/title[color% object% ()]{

A color is an object representing a red-green-blue (RGB) combination
 of primary colors plus an ``alpha'' for opacity. Each red, green, or
 blue component of the color is an exact integer in the range 0 to
 255, inclusive, and the alpha value is a real number between 0 and 1,
 inclusive.  For example, (0, 0, 0, 1.0) is solid black, (255, 255,
 255, 1.0) is solid white, (255, 0, 0, 1.0) is solid red, and (255, 0,
 0, 0.5) is translucent red.

See @racket[color-database<%>] for information about obtaining a color
object using a color name, and see also @racket[make-color].


@defconstructor*/make[(()
                       ([red byte?] [green byte?] [blue byte?]
                                    [alpha (real-in 0 1) 1.0])
                       ([color-name-or-obj (or/c string? (is-a?/c color%))]))]{

  Creates a new color.

  If three or four arguments are supplied to the constructor, the
  color is created with those RGB and alpha values.

  If a single @racket[color%] object is supplied, the color
  is created with the same RGB and alpha values as the given
  color.

  If a string is supplied, then it is passed to the
  @racket[color-database<%>]'s @method[color-database<%> find-color]
  method to find a color (signaling an error if the color is not in
  the @racket[color-database<%>]'s @method[color-database<%> get-names]
  method's result).

  If no arguments are supplied, the new color is black.
}

@defmethod[(red) byte?]{
  Returns the red component of the color.}

@defmethod[(green) byte?]{
  Returns the green component of the color.}

@defmethod[(blue) byte?]{
  Returns the blue component of the color.}

@defmethod[(alpha) (real-in 0 1)]{
  Returns the alpha component (i.e., opacity) of the color.}

@defmethod[(set [red byte?] [green byte?] [blue byte?]
                [alpha (real-in 0 1) 1.0])
           void?]{
  Sets the four (red, green, blue, and alpha) component values of the color.}

@defmethod[(copy-from [src (is-a?/c color%)]) (is-a?/c color%)]{
  Copies the RGB values of another color object to this one, returning
  this object as the result.}

@defmethod[(is-immutable?) boolean?]{
  Returns @racket[#t] if the color object is immutable.

  See also @racket[make-color] and @xmethod[color-database<%> find-color].}

@defmethod[(ok?) #t]{ 
  Returns @racket[#t] to indicate that the color object is valid.

  (Historically, the result could be @racket[#f], but color objects 
  are now always valid.)}

}
