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
                       ([color-name string?]))]{

Creates a new color with the given RGB values and alpha, or matching
 the given color name (using @racket["black"] if no color is given or if the
 name is not recognized). See @racket[color-database<%>] for more
 information on color names.

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
