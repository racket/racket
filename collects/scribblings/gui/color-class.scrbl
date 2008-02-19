#lang scribble/doc
@(require "common.ss")

@defclass/title[color% object% ()]{

A color is an object representing a red-green-blue (RGB) combination
 of primary colors, and is used to determine drawing colors. Each red,
 green, or blue component of the color is in the range 0 to 255,
 inclusive.  For example, (0, 0, 0) is black, (255, 255, 255) is
 white, and (255, 0, 0) is red.

See @scheme[color-database<%>] for information about obtaining a color
object using a color name.


@defconstructor*/make[(([red (integer-in 0 255)]
                        [green (integer-in 0 255)]
                        [blue (integer-in 0 255)])
                       ([color-name string?]))]{

Creates a new color with the given RGB values, or matching the given
 color name (using ``black'' if the name is not recognized). See
 @scheme[color-database<%>] for more information on color names.

}

@defmethod[(blue)
           (integer-in 0 255)]{

Returns the blue component of the color.

}

@defmethod[(copy-from [src (is-a?/c color%)])
           (is-a?/c color%)]{

Copies the RGB values of another color object to this one, returning
 this object as the result.

}

@defmethod[(green)
           (integer-in 0 255)]{

Returns the green component of the color.

}

@defmethod[(ok?)
           boolean?]{

Returns @scheme[#t] if the color object is valid.

}

@defmethod[(red)
           (integer-in 0 255)]{

Returns the red component of the color.

}

@defmethod[(set [red (integer-in 0 255)]
                [green (integer-in 0 255)]
                [blue (integer-in 0 255)])
           void?]{

Sets the three (red, green, and blue) component values of the color.
}}

