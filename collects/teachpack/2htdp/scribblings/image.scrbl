#lang scribble/doc

@(require (for-label (only-in scheme/contract and/c or/c any/c not/c)
                     2htdp/image
                     (except-in lang/htdp-beginner make-posn posn? posn-x posn-y image?)
                     lang/posn
                     scheme/gui/base)
          lang/posn
          "shared.ss"
          "image-util.ss"
          scribble/manual)

@teachpack["image"]{Images}

@defmodule[#:require-form beginner-require 2htdp/image]

The image teachpack provides a number of basic image construction functions, along with
combinators for building more complex images out of existing images. Basic images include
various polygons, ellipses and circles, and text, as well as bitmaps (typically bitmaps 
come about via the @onscreen{Insert Image...} menu item in DrScheme).
Existing images can be rotated, scaled, and overlaid on top of each other.

@section{Basic Images}

@defproc[(circle [radius (and/c real? (not/c negative?))]
                 [mode mode?]
                 [color color?])
         image?]{
  Constructs a circle with the given radius, height, mode, and color.
  
  @image-examples[(circle 30 "outline" "red")
                  (circle 20 "solid" "blue")]
  
}


@defproc[(ellipse [width (and/c real? (not/c negative?))]
                  [height (and/c real? (not/c negative?))]
                  [mode mode?] 
                  [color color?]) image?]{
  Constructs an ellipsis with the given width, height, mode, and color.
  
  @image-examples[(ellipse 40 20 "outline" "black")
                  (ellipse 20 40 "solid" "blue")]
  
}

@defproc[(triangle [side-length (and/c real? (not/c negative?))] 
                   [mode mode?]
                   [color color?])
         image?]{
  Constructs a upward-pointing equilateral triangle. 
  The @scheme[side-length] argument 
  determines the 
  length of the side of the triangle.

@image-examples[(triangle 40 "solid" "tan")]
}

@defproc[(right-triangle [side-length1 (and/c real? (not/c negative?))]
                         [side-length2 (and/c real? (not/c negative?))]
                         [mode mode?]
                         [color color?])
         image?]{
                 
  Constructs a triangle with a right angle where the two sides adjacent
  to the right angle have lengths @scheme[side-length1] and @scheme[side-length2].

  @image-examples[(right-triangle 36 48 "solid" "black")]
}
                
@defproc[(isosceles-triangle [side-length (and/c real? (not/c negative?))] 
                             [angle angle?]
                             [mode mode?]
                             [color color?])
         image?]{

 Creates a triangle with two equal-length sides, of length @scheme[side-length]
 where the angle between those sides is @scheme[angle]. The third
 leg is straight, horizontally. If the angle is less than 
 @scheme[180], then the triangle will point up and if the @scheme[angle]
 is more, then the triangle will point down. 
 
 @image-examples[(isosceles-triangle 200 170 "solid" "seagreen")
                 (isosceles-triangle 60 30 "solid" "aquamarine")
                 (isosceles-triangle 60 330 "solid" "lightseagreen")]
}


@defproc[(square [side-length (and/c real? (not/c negative?))]
                 [mode mode?]
                 [color color?])
         image?]{

 Constructs a square.
 
 @image-examples[(square 40 "solid" "slateblue")
                 (square 50 "outline" "darkmagenta")]

}

@defproc[(rectangle [width real?] [height real?] [mode mode?] [color color?]) image?]{
  Constructs a rectangle with the given width, height, mode, and color.
  @image-examples[(rectangle 40 20 "outline" "black")
                  (rectangle 20 40 "solid" "blue")]
}

@defproc[(rhombus [side-length (and/c real? (not/c negative?))]
                  [angle angle?]
                  [mode mode?]
                  [color color?])
         image?]{
                 
Constructs a four sided polygon with all equal sides and thus where opposite angles are equal to each
other. The top and bottom pair of angles is @scheme[angle] and the left and right are @scheme[(- 180 angle)].

@image-examples[(rhombus 40 45 "solid" "magenta")
                (rhombus 80 150 "solid" "mediumpurple")]
}

@defproc[(regular-polygon [side-length (and/c real? (not/c negative?))] 
                          [side-count side-count?]
                          [mode mode?]
                          [color color?])
         image?]{
  Constructs a regular polygon with @scheme[side-count] sides.

  @image-examples[(regular-polygon 50 3 "outline" "red")
                  (regular-polygon 40 4 "outline" "blue")
                  (regular-polygon 20 8 "solid" "red")]
}

@defproc[(star [side-length (and/c real? (not/c negative?))] 
               [mode mode?]
               [color color?])
         image?]{
  Constructs a star with five points. The @scheme[side-length] argument 
  determines the side length of the enclosing pentagon.

  @image-examples[(star 40 "solid" "gray")]
  
}

@defproc[(star-polygon [side-length (and/c real? (not/c negative?))]
                       [side-count side-count?]
                       [step-count step-count?]
                       [mode mode?]
                       [color color?])
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
                
@defproc[(polygon [verticies (listof posn?)] 
                  [mode mode?]
                  [color color?])
         image?]{
  Constructs a polygon connecting the given verticies.
  
  @image-examples[(polygon (list (make-posn 0 0)
                                 (make-posn -10 20)
                                 (make-posn 60 0)
                                 (make-posn -10 -20))
                           "solid" "burlywood")
                  (polygon (list (make-posn 0 0)
                                 (make-posn 0 40)
                                 (make-posn 20 40)
                                 (make-posn 20 60)
                                 (make-posn 40 60)
                                 (make-posn 40 20)
                                 (make-posn 20 20)
                                 (make-posn 20 0))
                           "solid" "plum")]
}

@defproc[(line [x1 real?] [y1 real?] [color color?]) image?]{
  Constructs an image representing a line segment that connects the points
  (0,0) to (x1,y1).
  
  @image-examples[(line 30 30 "black")
                  (line -30 20 "red")
                  (line 30 -20 "red")]
}

@defproc[(add-line [image image?]
                   [x1 real?] [y1 real?]
                   [x2 real?] [y2 real?]
                   [color color?])
         image?]{

  Adds a line to the image @scheme[image], starting from the point (@scheme[x1],@scheme[y1])
  and going to the point (@scheme[x2],@scheme[y2]).
  
  @image-examples[(add-line (ellipse 40 40 "outline" "maroon")
                            0 40 40 0 "maroon")
                  (add-line (ellipse 80 60 "outline" "darkolivegreen")
                            (+ 40 (* 40 (cos (* pi 1/4))))
                            (+ 30 (* 30 (sin (* pi 1/4))))
                            (+ 40 (* 40 (cos (* pi 5/4))))
                            (+ 30 (* 30 (sin (* pi 5/4))))
                            "darkolivegreen")]
}

@defproc[(text [string string?] [font-size (and/c integer? (<=/c 1 255))] [color color?])
         image?]{
                
  Constructs an image that draws the given string, using the font size and color.
                 
  @image-examples[(text "Hello" 24 "olive")
                  (text "Goodbye" 36 "indigo")]
}

@defproc[(text/font [string string?] [font-size (and/c integer? (<=/c 1 255))] [color color?]
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
  determines if the face is underlined. For more details on these arguments, see @scheme[font%],
  which ultimately is what this code uses to draw the font.
                 
  @image-examples[(text/font "Hello" 24 "olive"
                             "Gill Sans" 'swiss 'normal 'bold #f)
                  (text/font "Goodbye" 18 "indigo"
                             #f 'modern 'italic 'normal #f)
                  (text/font "not really a link" 18 "blue"
                             #f 'roman 'normal 'normal #t)]
}

@defform/subs[(bitmap bitmap-spec)
              ([bitmap-spec rel-string
                            id])]{
                                  
  Loads the bitmap specified by @scheme[bitmap-spec]. If @scheme[bitmap-spec] is a string, it is treated as a 
  relative path. If it is an identifier, it is treated like a require spec and used to refer to a file
  in a collection.
  
  @image-examples[(bitmap icons/stop-16x16.png)
                  (bitmap icons/b-run.png)]
}
                
@section{Overlaying Images}

@defproc[(overlay [i1 image?] [i2 image?] [is image?] ...) image?]{
  Overlays all of its arguments building a single image. The first argument goes
  on top of the second argument, which goes on top of the third argument, etc.
  The images are all lined up on their upper-left corners.

  @image-examples[(overlay (rectangle 30 60 "solid" "orange")
                           (ellipse 60 30 "solid" "purple"))
                  (overlay (ellipse 10 10 "solid" "red")
                           (ellipse 20 20 "solid" "black")
                           (ellipse 30 30 "solid" "red")
                           (ellipse 40 40 "solid" "black")
                           (ellipse 50 50 "solid" "red")
                           (ellipse 60 60 "solid" "black"))]
  
  }

@defproc[(overlay/align [x-place x-place?] [y-place y-place?] [i1 image?] [i2 image?] [is image?] ...) image?]{
  Overlays all of its image arguments, much like the @scheme[overlay] function, but using
  @scheme[x-place] and @scheme[y-place] to determine where the images are lined up. For example, if
  @scheme[x-place] and @scheme[y-place] are both @scheme["middle"], then the images are lined up
  on their centers.

  @image-examples[(overlay/align "middle" "middle"
                                 (rectangle 30 60 "solid" "orange")
                                 (ellipse 60 30 "solid" "purple"))
                  (overlay/align "right" "bottom"
                                 (rectangle 20 20 "solid" "silver")
                                 (rectangle 30 30 "solid" "seagreen")
                                 (rectangle 40 40 "solid" "silver")
                                 (rectangle 50 50 "solid" "seagreen"))]
                                  
  
  }

@defproc[(overlay/xy [i1 image?] [x real?] [y real?] [i2 image?]) image?]{
  Constructs an image by overlaying @scheme[i1] on top of @scheme[i2] after
  shifting @scheme[i2] over by @scheme[x] pixels to the right and @scheme[y] 
  pixels down.
  @image-examples[(overlay/xy (rectangle 20 20 "outline" "black")
                              20 0
                              (rectangle 20 20 "outline" "black"))
                  (overlay/xy (rectangle 20 20 "solid" "red")
                              20 20
                              (rectangle 20 20 "solid" "black"))
                  (overlay/xy (rectangle 20 20 "solid" "red")
                              -20 -20
                              (rectangle 20 20 "solid" "black"))
                  (overlay/xy 
                   (overlay/xy (ellipse 40 40 "outline" "black")
                               10
                               15
                               (ellipse 10 10 "solid" "forestgreen"))
                   20
                   15
                   (ellipse 10 10 "solid" "forestgreen"))]
}

@defproc[(underlay [i1 image?] [i2 image?] [is image?] ...) image?]{
  Underlays all of its arguments building a single image.
  
  It behaves like @scheme[overlay], but with the arguments in the reverse order.
  That is, the first argument goes
  underneath of the second argument, which goes underneath the third argument, etc.
  The images are all lined up on their upper-left corners.

  @image-examples[(underlay (rectangle 30 60 "solid" "orange")
                            (ellipse 60 30 "solid" "purple"))
                  (underlay (ellipse 10 60 "solid" "red")
                            (ellipse 20 50 "solid" "black")
                            (ellipse 30 40 "solid" "red")
                            (ellipse 40 30 "solid" "black")
                            (ellipse 50 20 "solid" "red")
                            (ellipse 60 10 "solid" "black"))]
  
  }

@defproc[(underlay/align [x-place x-place?] [y-place y-place?] [i1 image?] [i2 image?] [is image?] ...) image?]{
  Underlays all of its image arguments, much like the @scheme[underlay] function, but using
  @scheme[x-place] and @scheme[y-place] to determine where the images are lined up. For example, if
  @scheme[x-place] and @scheme[y-place] are both @scheme["middle"], then the images are lined up
  on their centers.

  @image-examples[(underlay/align "middle" "middle"
                                  (rectangle 30 60 "solid" "orange")
                                  (ellipse 60 30 "solid" "purple"))
                  (underlay/align "right" "top"
                                  (rectangle 50 50 "solid" "seagreen")
                                  (rectangle 40 40 "solid" "silver")
                                  (rectangle 30 30 "solid" "seagreen")
                                  (rectangle 20 20 "solid" "silver"))]
                                  
  
  }

@defproc[(underlay/xy [i1 image?] [x real?] [y real?] [i2 image?]) image?]{
  Constructs an image by underlaying @scheme[i1] underneath of @scheme[i2] after
  shifting @scheme[i2] over by @scheme[x] pixels to the right and @scheme[y] 
  pixels down.
  
  This is the same as @scheme[(overlay/xy i2 (- x) (- y) i1)].
  
  @image-examples[(underlay/xy (rectangle 20 20 "outline" "black")
                               20 0
                               (rectangle 20 20 "outline" "black"))
                  (underlay/xy (rectangle 20 20 "solid" "red")
                               20 20
                               (rectangle 20 20 "solid" "black"))
                  (underlay/xy (rectangle 20 20 "solid" "red")
                               -20 -20
                               (rectangle 20 20 "solid" "black"))
                  (underlay/xy 
                   (underlay/xy (ellipse 40 40 "solid" "gray")
                                10
                                15
                                (ellipse 10 10 "solid" "forestgreen"))
                   20
                   15
                   (ellipse 10 10 "solid" "forestgreen"))]
}


@defproc[(beside [i1 image?] [i2 image?] [is image?] ...) image?]{
  Constructs an image by placing all of the argument images in a
  horizontal row, aligned along their top edges.

  @image-examples[(beside (ellipse 20 70 "solid" "gray")
                          (ellipse 20 50 "solid" "darkgray")
                          (ellipse 20 30 "solid" "dimgray")
                          (ellipse 20 10 "solid" "black"))]
                          
  
  }

@defproc[(beside/align [y-place y-place?] [i1 image?] [i2 image?] [is image?] ...) image?]{
  Constructs an image by placing all of the argument images in a horizontal row, lined
  up as indicated by the @scheme[y-place] argument. For example, if @scheme[y-place]
  is @scheme["middle"], then the images are placed side by side with their centers 
  lined up with each other.

  @image-examples[(beside/align "bottom"
                                (ellipse 20 70 "solid" "lightsteelblue")
                                (ellipse 20 50 "solid" "mediumslateblue")
                                (ellipse 20 30 "solid" "slateblue")
                                (ellipse 20 10 "solid" "navy"))
                  
                  (beside/align "center"
                                (ellipse 20 70 "solid" "mediumorchid")
                                (ellipse 20 50 "solid" "darkorchid")
                                (ellipse 20 30 "solid" "purple")
                                (ellipse 20 10 "solid" "indigo"))
                  
                  (beside/align "baseline"
                                (text "ijy" 18 "black")
                                (text "ijy" 24 "black"))]
                                 
  
  }


@defproc[(above [i1 image?] [i2 image?] [is image?] ...) image?]{
  Constructs an image by placing all of the argument images in a
  vertical row, aligned along their left edges.

  @image-examples[(above (ellipse 70 20 "solid" "gray")
                         (ellipse 50 20 "solid" "darkgray")
                         (ellipse 30 20 "solid" "dimgray")
                         (ellipse 10 20 "solid" "black"))]
                          
  
  }

@defproc[(above/align [y-place y-place?] [i1 image?] [i2 image?] [is image?] ...) image?]{
  Constructs an image by placing all of the argument images in a vertical row, lined
  up as indicated by the @scheme[x-place] argument. For example, if @scheme[x-place]
  is @scheme["middle"], then the images are placed above each other with their centers 
  lined up.

  @image-examples[(above/align "right"
                               (ellipse 70 20 "solid" "gold")
                               (ellipse 50 20 "solid" "goldenrod")
                               (ellipse 30 20 "solid" "darkgoldenrod")
                               (ellipse 10 20 "solid" "sienna"))
                  
                  (above/align "center"
                               (ellipse 70 20 "solid" "yellowgreen")
                               (ellipse 50 20 "solid" "olivedrab")
                               (ellipse 30 20 "solid" "darkolivegreen")
                               (ellipse 10 20 "solid" "darkgreen"))]
                                 
  
  }

@section{Rotating, Scaling, Cropping, and Framing Images}

@defproc[(rotate [angle angle?] [image image?]) image?]{
  Rotates @scheme[image] by @scheme[angle] degrees in a counter-clockwise direction.

          @image-examples[(rotate 45 (ellipse 60 20 "solid" "olivedrab"))
                          (rotate 5 (rectangle 50 50 "outline" "black"))
                          (rotate 45
                                  (beside/align
                                   "center"
                                   (rectangle 40 20 "solid" "darkseagreen")
                                   (rectangle 20 100 "solid" "darkseagreen")))]
          
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

@defproc[(crop [x real?] [y real?] 
               [width (and/c real? (not/c negative?))]
               [height (and/c real? (not/c negative?))]
               [image image?])
         image?]{

 Crops @scheme[image] to the rectangle with the upper left at the point (@scheme[x],@scheme[y])
 and with @scheme[width] and @scheme[height]. 
 
 @image-examples[(crop 0 0 40 40 (circle 40 "solid" "chocolate"))
                 (crop 40 60 40 60 (ellipse 80 120 "solid" "dodgerblue"))
                 (above
                  (beside (crop 40 40 40 40 (circle 40 "solid" "palevioletred"))
                          (crop 0 40 40 40 (circle 40 "solid" "lightcoral")))
                  (beside (crop 40 0 40 40 (circle 40 "solid" "lightcoral"))
                          (crop 0 0 40 40 (circle 40 "solid" "palevioletred"))))]
                 
}

@defproc[(frame [image image?]) image?]{
  Returns an image just like @scheme[image], except
  with a black, single pixel frame drawn around the 
  bounding box of the image.
  
  @image-examples[(frame (ellipse 20 20 "outline" "black"))]
  
  Generally speaking, this function is useful to 
  debug image constructions, i.e., to see where
  certain sub-images appear within some larger image.
  
  @image-examples[(beside/align "bottom"
                                (ellipse 20 70 "solid" "lightsteelblue")
                                (frame (ellipse 20 50 "solid" "mediumslateblue"))
                                (ellipse 20 30 "solid" "slateblue")
                                (ellipse 20 10 "solid" "navy"))]
}

@section{Image Properties}

@defproc[(image-width [i image?]) (and/c integer? positive? exact?)]{
  Returns the width of @scheme[i].
                       
  @image-examples[(image-width (ellipse 30 40 "solid" "orange"))
                  (image-width (circle 30 "solid" "orange"))
                  (image-width (beside (circle 20 "solid" "orange")
                                       (circle 20 "solid" "purple")))]
}

@defproc[(image-height [i image?]) (and/c integer? positive? exact?)]{
  Returns the height of @scheme[i].
  
  @image-examples[(image-height (ellipse 30 40 "solid" "orange"))
                  (image-height (circle 30 "solid" "orange"))
                  (image-height (overlay (circle 20 "solid" "orange")
                                         (circle 30 "solid" "purple")))]
  }

@defproc[(image-baseline [i image?]) (and/c integer? positive? exact?)]{
  Returns the distance from the top of the image to its baseline. 
  Unless the image was constructed with @scheme[text] or @scheme[text/font],
  this will be the same as its height.
  
  @image-examples[(image-baseline (text "Hello" 24 "black"))
                  (image-height (text "Hello" 24 "black"))
                  (image-baseline (rectangle 100 100 "solid" "black"))
                  (image-height (rectangle 100 100 "solid" "black"))]
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

@defproc[(color? [x any/c]) boolean?]{

  Determines if @scheme[x] represents a color. Both strings and symbols are allowed as colors.
  For example,
  @scheme["magenta"], @scheme["black"], @scheme['orange], and @scheme['purple]
  are allowed. Colors are not case-sensitive, so 
  @scheme["Magenta"], @scheme["Black"], @scheme['Orange], and @scheme['Purple]
  are also allowed, and are the same colors as in the previous sentence.

  If a color is not recognized, black is used in its place.
  
  The complete list of colors is available in the documentation for
  @scheme[color-database<%>].
                                      
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

The baseline of an image is the place where the bottoms any letters line up, not counting descenders, e.g. the tail on ``y'' or ``g'' or ``j''.


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

Two images are equal if they draw exactly the same way, at their current size
(not neccessarily at all sizes).

@;{
Image equality testing is done structurally, i.e., based on 
the construction of the image, 
although with certain, expected equivalences. For example, 
two rectangles with the same width, height, color, and mode
are equal. Similarly, constructing a 20x10 rectangle and
then rotating it by 90 degress is equal to a 10x20 rectangle
(provided they have the same color and mode).

Equality testing may contain a few nuances, though:
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
         or @scheme[check-within] for test suites involving images.}
   
   @item{Combining a series of line segments to form a polygon produces
         an image that is different than the polygon.}
   
   @item{In order to make equality on images created with 
         @scheme[text] and @scheme[text/font]
         work well, each string passed to either of those functions results
         in a number of horizontally aligned images, one for each letter in the
         string. This means that, for example
         @schemeblock[(equal? (beside/align "baseline"
                                            (text "a" 18 "black")
                                            (text "b" 18 "black"))
                              (text "ab" 18 "black"))]
         is true, but that subtle aspects of font drawing may be wrong, since
         the underlying toolkit only gets a single letter at a time, instead
         of the entire word (or sentence).
         
         The most obvious way that this shows up is in the handling of ligatures.
         For example, the letter combinations ``ff'' and ``fi'' and ``fl'' are
         generally drawn intertwined when they appear together, and thus an ``f''
         drawn separately from an ``i'' looks different than the ligature ``fi''.
         For example, here is how 24 point Times font looks when the word ``refill''
         is drawn, first with ligatures and then without:
         @centerline{@image["2htdp/scribblings/ligature.png"]}.
         }
]
}