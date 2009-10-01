#lang scribble/doc

@(require (for-label (except-in 2htdp/image image?)
                     lang/htdp-beginner)
          "shared.ss"
          scribble/manual)

@teachpack["image"]{Images}

@defmodule[#:require-form beginner-require 2htdp/image]

The image teachpack provides a number of basic image construction functions, along with
combinators for building more complex images out of existing images. It includes
support for various polygons, ellipses and circles, and text, as well as supporting bitmaps.
Images can be rotated, scaled, and overlaid on top of each other, as described below.

@emph{This library is currently a work in progress. 
      I don't expect the existing primitives to change, but more will be added
      (and bugs will be fixed...).}

@section{Basic Images}

@defproc[(ellipse [width real?] [height real?] [mode mode?] [color (or/c symbol? string?)]) image?]{
  Constructs an ellipsis with the given width, height, mode, and color.
}

@defproc[(rectangle [width real?] [height real?] [mode mode?] [color (or/c symbol? string?)]) image?]{
  Constructs a rectangle with the given width, height, mode, and color.
}

@section{Overlaying Images}

@defproc[(overlay [i1 image?] [i2 image?] [is image?] ...) image?]{
  Overlays all of its arguments building a single image. The first argument goes
  on top of the second argument, which goes on top of the third argument, etc.
  The images are all lined up on their upper-right corners
}

@defproc[(overlay/places [x-place x-place?] [y-place y-place?] [i1 image?] [i2 image?] [is image?]) image?]{
  Overlays all of its image arguments, much like the @scheme[overlay] function, but using
  @scheme[x-place] and @scheme[y-place] to determine where the images are lined up. For example, if
  @scheme[x-place] and @scheme[y-place] are both @scheme["middle"], then the images are lined up
  on their centers.
}

@defproc[(overlay/xy [i1 image?] [x real?] [y real?] [i2 image?]) image?]{
  Constructs an image by overlaying @scheme[i1] on top of @scheme[i2] after
  shifting @scheme[i2] over by @scheme[x] pixels to the right and @scheme[y] 
  pixels down.
}

@defproc[(beside [i1 image?] [i2 image?] [is image?] ...) image?]{
  Constructs an image by placing all of the argument images in a
  horizontal row, aligned along their top edges.
}

@defproc[(beside/places [y-place y-place?] [i1 image?] [i2 image?] [is image?] ...) image?]{
  Constructs an image by placing all of the argument images in a horizontal row, lined
  up as indicated by the @scheme[y-place] argument. For example, if @scheme[y-place]
  is @scheme["middle"], then the images are placed side by side with their centers 
  lined up with each other.
}

@section{Rotating and Framing Images}

@defproc[(rotate [angle angle?] [image image?]) image?]{
  Rotates @scheme[image] by @scheme[angle] degrees.                                                        
}

@defproc[(frame [image image?]) image?]{
  Returns an image just like @scheme[image], except
  with a black, single pixel frame drawn around the 
  bounding box of the image.
  
  Generally speaking, this function is useful to 
  debug image constructions, i.e., to see where
  certain sub-images appear within some larger image.
}

@section{Image Predicates}

This section lists predicates for the basic structures provided by the image library.

@defproc[(image? [x any/c]) boolean?]{
 Determines if @scheme[x] is an image. Images are returned by functions
 like @scheme[ellipse] and @scheme[rectangle] and
 accepted by functions like @scheme[overlay] and @scheme[beside].
}

@defproc[(mode? [x any/c]) boolean?]{
 Determines if @scheme[x] is a mode suitable for
 constructing images. It can be one of
 @scheme['solid], @scheme["solid"], @scheme['outline],
 or @scheme["outline"], indicating if the shape is
 filled in or not.
}

@defproc[(y-place? [x any/c]) boolean?]{
  Determines if @scheme[x] is a placement option
  for the vertical direction. It can be one
  of
@scheme["top"],
@scheme['top], 
@scheme["bottom"],
@scheme['bottom],
@scheme["middle"],
@scheme['middle],
@scheme["center"],
@scheme['center],
@scheme["baseline"], or
@scheme['baseline].
}

@defproc[(x-place? [x any/c]) boolean?]{
  Determines if @scheme[x] is a placement option
  for the horizontal direction. It can be one
  of @scheme["left"],
  @scheme['left], 
  @scheme["right"],
  @scheme['right],
  @scheme["middle"],
  @scheme['middle],
  @scheme["center"], or
  @scheme['center].
}

@defproc[(angle? [x any/c]) boolean?]{
  Determines if @scheme[x] is an angle, namely
  a real number between @scheme[0] (inclusive)
  and @scheme[360] (exclusive).
}

@section{Equality Testing of Images}

Image equality testing is done structurally, i.e., based on 
the construction of the image, 
although with certain, expected equivalences. For example, 
two rectangles with the same width, height, color, and mode
are equal. Similarly, constructing a 20x10 rectangle and
then rotating it by 90 degress is equal to a 10x20 rectangle
(provided they have the same color and mode).

Equality testing contains a two surprises, though:
@itemize[
  @item{Overlaying two images in opposite orders is never equal. For example,
        these two images are not @scheme[equal]:
        @schemeblock[(overlay/xy (rectangle 30 10 "solid" "blue")
                                 0
                                 10
                                 (rectangle 30 10 "solid" "red"))]
        @schemeblock[(overlay/xy (rectangle 30 10 "solid" "red")
                                 0
                                 -10
                                 (rectangle 30 10 "solid" "blue"))]
        even thought they may appear to be the same when drawn. 
        
        The rationale for them being different is that, at some scale factor,
        they will draw differently; specifically when they are scaled down
        far enough, the first will appear to be a single red pixel and the second will appear to
        be a single blue pixel.}
   @item{When rotating images, the internal calculations involve real numbers, not just
         rationals and thus must be approximated with Scheme's inexact numbers, causing
         small roundoff errors that make the images draw slightly differently. 
         
         To combat this problem, use @scheme[equal~?] to compare the images,
         or @scheme[check-within] for test suites involving images.}]
