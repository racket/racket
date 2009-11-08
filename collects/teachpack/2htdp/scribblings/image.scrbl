#lang scribble/doc

@(require (for-label (except-in 2htdp/image image?)
                     lang/htdp-beginner
                     scheme/gui/base)
          "shared.ss"
          "image-util.ss"
          scribble/manual)

@teachpack["image"]{Images}

@defmodule[#:require-form beginner-require 2htdp/image]

The image teachpack provides a number of basic image construction functions, along with
combinators for building more complex images out of existing images. Basic images include
various polygons, ellipses and circles, and text, as well as bitmaps (typically bitmaps 
come about via the @onscreen{Insert Image...} menu item in DrScheme.
Existing images can be rotated, scaled, and overlaid on top of each other.

@emph{This library is currently a work in progress. 
      I don't expect the existing primitives to change, but more will be added
      (and bugs will be fixed...).}

@section{Basic Images}

@defproc[(circle [radius (and/c real? positive?)] 
                 [mode mode?]
                 [color (or/c symbol? string?)])
         image?]{
  Constructs a circle with the given radius, height, mode, and color.
  
  @image-examples[(circle 30 "outline" "red")
                  (circle 20 "solid" "blue")]
  
}


@defproc[(ellipse [width (and/c real? positive?)] [height (and/c real? positive?)] [mode mode?] [color (or/c symbol? string?)]) image?]{
  Constructs an ellipsis with the given width, height, mode, and color.
  
  @image-examples[(ellipse 40 20 "outline" "black")
                  (ellipse 20 40 "solid" "blue")]
  
}

@defproc[(rectangle [width real?] [height real?] [mode mode?] [color (or/c symbol? string?)]) image?]{
  Constructs a rectangle with the given width, height, mode, and color.
  @image-examples[(rectangle 40 20 "outline" 'black)
                  (rectangle 20 40 "solid" 'blue)]
}

@defproc[(regular-polygon [side-length (and/c positive? real?)] 
                          [side-count side-count?]
                          [mode mode?]
                          [color (or/c symbol? string?)])
         image?]{
  Constructs a regular polygon with @scheme[side-count] sides.

  @image-examples[(regular-polygon 30 3 "outline" "red")
                  (regular-polygon 20 4 "outline" "blue")
                  (regular-polygon 20 6 "solid" "red")]
}

@defproc[(triangle [side-length (and/c positive? real?)] 
                   [mode mode?]
                   [color (or/c symbol? string?)])
         image?]{
  Constructs a upward-pointing equilateral triangle. 
  The @scheme[side-length] argument 
  determines the 
  length of the side of the triangle.

@image-examples[(triangle 40 "solid" "tan")]
}

@defproc[(star [side-length (and/c real? positive?)] 
               [mode mode?]
               [color (or/c symbol? string?)])
         image?]{
  Constructs a star with five points. The @scheme[side-length] argument 
  determines the side length of the enclosing pentagon.

  @image-examples[(star 40 "solid" "gray")]
  
}

@defproc[(star-polygon [side-length (and/c real? positive?)]
                       [side-count side-count?]
                       [step-count step-count?]
                       [mode mode?]
                       [color (or/c symbol? string?)])
         image?]{
 
  Constructs an arbitrary regular star polygon (a generalization of the regular polygons). 
  The polygon is enclosed by a regular polygon with @scheme[side-count] sides each
  @scheme[side-length] long. The polygon is actually constructed by going from vertex to
  vertex around the regular polgon, but skipping over every @scheme[step-count] verticies.
  
  For examples, if @scheme[side-count] is @scheme[5] and @scheme[step-count] is @scheme[2],
  then this function produces a shape just like @scheme[star].
  
  @image-examples[(star-polygon 40 5 2 "solid" "seagreen")
                  (star-polygon 40 7 3 "outline" "darkred")
                  (star-polygon 20 10 3 "solid" "cornflowerblue")]
 
}

@defproc[(text [string string?] [font-size (and/c integer? (<=/c 1 255))] [color (or/c symbol? string?)])
         image?]{
                
  Constructs an image that draws the given string, using the font size and color.
                 
  @image-examples[(text "Hello" 24 "olive")
                  (text "Goodbye" 36 "indigo")]
}

@defproc[(text/font [string string?] [font-size (and/c integer? (<=/c 1 255))] [color (or/c symbol? string?)]
                    [face (or/c string? #f)]
                    [family (or/c 'default 'decorative 'roman 'script 'swiss 'modern 'symbol 'system)]
                    [style (or/c 'normal 'italic 'slant)]
                    [weight (or/c 'normal 'bold 'light)]
                    [underline? any/c])
         image?]{
                
  Constructs an image that draws the given string, using a complete font specification.
  
  The @scheme[face] and the @scheme[family] combine to give the complete typeface. If 
  @scheme[face] is available on the system, it is used, but if not then a default typeface
  based on the @scheme[family] is chosen. The @scheme[style] controls if the face is italic
  or not (under Windows and Mac OS X, @scheme['slant] and @scheme['italic] are the same),
  the @scheme[weight] controls if it is boldface (or light), and @scheme[underline?]
  determines if the face is underlined. For more details on these arguments, see @scheme[face%],
  which ultimately is what this code uses to draw the font.
                 
  @image-examples[(text/font "Hello" 24 "olive"
                             "Gill Sans" 'swiss 'normal 'bold #f)
                  (text/font "Goodbye" 18 "indigo"
                             #f 'modern 'italic 'normal #f)
                  (text/font "not really a link" 18 "blue"
                             #f 'roman 'normal 'normal #t)]
}

                
@section{Overlaying Images}

@defproc[(overlay [i1 image?] [i2 image?] [is image?] ...) image?]{
  Overlays all of its arguments building a single image. The first argument goes
  on top of the second argument, which goes on top of the third argument, etc.
  The images are all lined up on their upper-right corners

  @image-examples[(overlay (ellipse 60 30 "solid" "purple")
                           (rectangle 30 60 "solid" "orange"))
                  (overlay (ellipse 10 10 "solid" "red")
                           (ellipse 30 30 "solid" "black")
                           (ellipse 50 50 "solid" "red")
                           (ellipse 70 70 "solid" "black"))]
  
  }

@defproc[(overlay/places [x-place x-place?] [y-place y-place?] [i1 image?] [i2 image?] [is image?]) image?]{
  Overlays all of its image arguments, much like the @scheme[overlay] function, but using
  @scheme[x-place] and @scheme[y-place] to determine where the images are lined up. For example, if
  @scheme[x-place] and @scheme[y-place] are both @scheme["middle"], then the images are lined up
  on their centers.

  @image-examples[(overlay/places "middle" "middle"
                                  (rectangle 30 60 "solid" "orange")
                                  (ellipse 60 30 "solid" "purple"))
                  (overlay/places "right" "bottom"
                                  (rectangle 20 20 "solid" "red")
                                  (rectangle 30 30 "solid" "black")
                                  (rectangle 40 40 "solid" "red")
                                  (rectangle 50 50 "solid" "black"))]
                                  
  
  }

@defproc[(overlay/xy [i1 image?] [x real?] [y real?] [i2 image?]) image?]{
  Constructs an image by overlaying @scheme[i1] on top of @scheme[i2] after
  shifting @scheme[i2] over by @scheme[x] pixels to the right and @scheme[y] 
  pixels down.
  @image-examples[(overlay/xy (ellipse 40 40 "outline" "black")
                              25 
                              25
                              (ellipse 10 10 "solid" "forestgreen"))
                  (overlay/xy (rectangle 10 10 "outline" "red")
                              10 0
                              (rectangle 10 10 "outline" "black"))
                  (overlay/xy (rectangle 10 10 "solid" "red")
                              10 10
                              (rectangle 10 10 "solid" "black"))
                  (overlay/xy (rectangle 10 10 "solid" "red")
                              -10 -10
                              (rectangle 10 10 "solid" "black"))]
}

@defproc[(beside [i1 image?] [i2 image?] [is image?] ...) image?]{
  Constructs an image by placing all of the argument images in a
  horizontal row, aligned along their top edges.

  @image-examples[(beside (ellipse 20 70 "solid" "gray")
                          (ellipse 20 50 "solid" "darkgray")
                          (ellipse 20 30 "solid" "dimgray")
                          (ellipse 20 10 "solid" "black"))]
                          
  
  }

@defproc[(beside/places [y-place y-place?] [i1 image?] [i2 image?] [is image?] ...) image?]{
  Constructs an image by placing all of the argument images in a horizontal row, lined
  up as indicated by the @scheme[y-place] argument. For example, if @scheme[y-place]
  is @scheme["middle"], then the images are placed side by side with their centers 
  lined up with each other.

  @image-examples[(beside/places "bottom"
                                 (ellipse 20 70 "solid" "lightsteelblue")
                                 (ellipse 20 50 "solid" "mediumslateblue")
                                 (ellipse 20 30 "solid" "slateblue")
                                 (ellipse 20 10 "solid" "navy"))
                  
                  (beside/places "center"
                                 (ellipse 20 70 "solid" "mediumorchid")
                                 (ellipse 20 50 "solid" "darkorchid")
                                 (ellipse 20 30 "solid" "purple")
                                 (ellipse 20 10 "solid" "indigo"))
                                                                  
                  (beside/places "baseline"
                                 (text "ijy" 18 "black")
                                 (text "ijy" 24 "black"))]
                                 
  
  }

@section{Rotating, Scaling, and Framing Images}

@defproc[(rotate [angle angle?] [image image?]) image?]{
  Rotates @scheme[image] by @scheme[angle] degrees.                                                        

          @image-examples[(rotate 45 (ellipse 60 20 "solid" "olivedrab"))
                          (rotate 5 (rectangle 50 50 "outline" "black"))]
          
}

@defproc[(scale [factor real?] [image image?]) image?]{
  Scales @scheme[image] by @scheme[factor]. 
         
         @image-examples[(scale 2 (ellipse 20 30 "solid" "blue"))
                         (ellipse 40 60 "solid" "blue")]
  
  
  
}

@defproc[(scale/xy [x-factor real?] [y-factor real?] [image image?]) image?]{
  Scales @scheme[image] by @scheme[x-factor] horizontally and by 
  @scheme[y-factor] vertically. 
  
  @image-examples[(scale/xy 3 
                            2 
                            (ellipse 20 30 "solid" "blue")) 
                  (ellipse 60 60 "solid" "blue")]
}


@defproc[(frame [image image?]) image?]{
  Returns an image just like @scheme[image], except
  with a black, single pixel frame drawn around the 
  bounding box of the image.
  
  @image-examples[(frame (ellipse 20 20 "outline" "black"))]
  
  Generally speaking, this function is useful to 
  debug image constructions, i.e., to see where
  certain sub-images appear within some larger image.
  
  @image-examples[(beside/places "bottom"
                                 (ellipse 20 70 "solid" "lightsteelblue")
                                 (frame (ellipse 20 50 "solid" "mediumslateblue"))
                                 (ellipse 20 30 "solid" "slateblue")
                                 (ellipse 20 10 "solid" "navy"))]
}

@section{Image Properties}

@defproc[(image-width [i image?]) (and/c number? positive?)]{
  Returns the width of @scheme[i].
}

@defproc[(image-height [i image?]) (and/c number? positive?)]{
  Returns the height of @scheme[i].
}

@section{Image Predicates}

This section lists predicates for the basic structures provided by the image library.

@defproc[(image? [x any/c]) boolean?]{
 Determines if @scheme[x] is an image. Images are returned by functions
 like @scheme[ellipse] and @scheme[rectangle] and
 accepted by functions like @scheme[overlay] and @scheme[beside].

 Additionally, images inserted into a DrScheme window are treated as
 bitmap images, as are instances of @scheme[image-snip%] and @scheme[bitmap%].
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

@defproc[(side-count? [x any/c]) boolean?]{
  Determines if @scheme[x] is an integer 
  greater than or equal to @scheme[3].
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
